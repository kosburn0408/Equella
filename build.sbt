import java.util.Properties

import com.typesafe.config.{Config, ConfigFactory}
import com.typesafe.sbt.license.LicenseReport
import org.jdom2.{DocType, Document, Element}
import org.jdom2.output.{Format, XMLOutputter}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.mutable

lazy val learningedge_config = project in file("Dev/learningedge-config")

lazy val allPlugins = LocalProject("allPlugins")

val legacyPaths = Seq(
  javaSource in Compile := baseDirectory.value / "src",
  javaSource in Test := baseDirectory.value / "test",
  unmanagedResourceDirectories in Compile := (baseDirectory.value / "resources") :: Nil,
  unmanagedSourceDirectories in Compile := (javaSource in Compile).value :: Nil,
  unmanagedSourceDirectories in Test := (javaSource in Test).value :: Nil
)

lazy val equellaserver = (project in file("Source/Server/equellaserver")).enablePlugins(JPFRunnerPlugin)

lazy val adminTool = (project in file("Source/Server/adminTool")).settings(legacyPaths).dependsOn(
  platformSwing,
  platformEquella,
  LocalProject("com_tle_webstart_admin"),
  LocalProject("adminConsoleJar")
)

lazy val conversion = (project in file("Source/Server/conversion")).settings(legacyPaths).dependsOn(
  platformCommon
)

lazy val UpgradeInstallation = (project in file("Source/Tools/UpgradeInstallation")).settings(legacyPaths).dependsOn(
  platformCommon,
  platformEquella,
  log4jCustom
)

lazy val UpgradeManager = (project in file("Source/Tools/UpgradeManager")).settings(legacyPaths).dependsOn(platformCommon, platformEquella, log4jCustom)

lazy val Installer = (project in file("Installer")).settings(legacyPaths).dependsOn(platformCommon, platformSwing, platformEquella, UpgradeManager)

lazy val equella = (project in file(".")).enablePlugins(JPFScanPlugin, JarSignerPlugin, GitVersioning)
  .aggregate(equellaserver, allPlugins, adminTool, Installer,
    UpgradeManager, conversion, UpgradeInstallation, learningedge_config)

buildConfig in ThisBuild := Common.buildConfig

oracleDriverJar in ThisBuild := {
  val c = buildConfig.value
  if (c.hasPath("build.oraclejar")) {
    Some(file(c.getString("build.oraclejar")))
  } else None
}

name := "Equella"

git.useGitDescribe := true

val TagRegex = """(.*)-(.*)-(\d*)-(.*)""".r
git.gitTagToVersionNumber := {
  case TagRegex(m, t, v, sha) => Some(EquellaVersion(m, t, v.toInt, sha).fullVersion)
  case _ => None
}

equellaVersion in ThisBuild := EquellaVersion(version.value)

versionProperties in ThisBuild := {
  val eqVersion = equellaVersion.value
  val props = new Properties
  props.putAll(
    Map("version.mm" -> eqVersion.majorMinor,
      "version.mmr" -> s"${eqVersion.majorMinor}.r${eqVersion.commits}",
      "version.display" -> s"${eqVersion.majorMinor}-${eqVersion.releaseType}",
      "version.commit" -> eqVersion.sha).asJava)
  val f = target.value / "version.properties"
  IO.write(props, "version", f)
  f
}

updateLicenses := {
  val ourOrg = organization.value
  val serverReport = (updateLicenses in equellaserver).value
  val plugsinReports = updateLicenses.all(ScopeFilter(inAggregates(allPlugins))).value
  val allLicenses = (plugsinReports.flatMap(_.licenses) ++ serverReport.licenses)
    .groupBy(_.module).values.map(_.head).filterNot(_.module.organization == ourOrg)
  LicenseReport(allLicenses.toSeq, serverReport.orig)
}

writeLanguagePack := {
  IO.withTemporaryDirectory { dir =>
    val allProps = langStrings.all(ScopeFilter(inAggregates(allPlugins, includeRoot = false))).value
      .flatten.groupBy(ls => (ls.group, ls.xml))
      .map { case ((g, xml), lss) =>
        val fname = g + (if (xml) ".xml" else ".properties")
        val f = dir / fname
        val p = new SortedProperties()
        lss.foreach(ls => p.putAll(ls.strings.asJava))
        Using.fileOutputStream()(f) { os =>
          if (xml) p.storeToXML(os, "") else p.store(os, "")
        }
        (f, fname)
      }
    val outZip = target.value / "reference-language-pack.zip"
    sLog.value.info(s"Writing ${outZip.absolutePath}")
    IO.zip(allProps, outZip)
    outZip
  }
}

aggregate in dumpLicenseReport := false

cancelable in Global := true

val pluginAndLibs = Def.task {
  val bd = baseDirectory.value
  val jpfLibs = jpfLibraryJars.value
  (bd, jpfLibs)
}

mergeJPF := {
  case class PluginDeets(bd: File, pId: String, rootElem: Element, imports: Seq[Element], libs: Classpath)
  case class LangString(group: String, key: String, pluginId: String, value: String)

  val keepPlugins = Set("com.tle.log4j", "com.tle.web.adminconsole")
  val basePlugin = baseDirectory.value / "Source/Plugins/Core/com.equella.core"
  IO.delete(basePlugin)
  val baseSrc = basePlugin / "src"
  val baseScalaSrc = basePlugin / "scalasrc"
  val baseRes = basePlugin / "resources"
  val allImports = pluginAndLibs.all(ScopeFilter(inAggregates(allPlugins, includeRoot = false))).value.map {
    case (f, l) =>
      val doc = Common.saxBuilder.build(f / "plugin-jpf.xml")
      val e = doc.getRootElement
      val imports = Option(e.getChild("requires")).toSeq.flatMap(e => e.getChildren("import").asScala)
        .filter(!_.getAttributeValue("plugin-id").contains(":"))
      PluginDeets(f, e.getAttributeValue("id"), e, imports, l)
  }
  val depMap = allImports.flatMap {
    case PluginDeets(_, pId, _, imps, _) => imps.map(_.getAttributeValue("plugin-id")) map {
      (_, pId)
    }
  }.groupBy(_._1).mapValues(_.map(_._2))

  val (allowed, others) = allImports.partition {
    case PluginDeets(bd, pId, r, _, l) =>
      val adminConsole = r.getChildren("attributes").asScala.flatMap(_.getChildren("attribute").asScala).find {
        _.getAttributeValue("id") == "type"
      }.exists(_.getAttributeValue("value") == "admin-console")
      l.isEmpty && !adminConsole && r.getChildren("extension-point").isEmpty &&
        !keepPlugins(pId) && !(bd / "build.sbt").exists
  }

  @tailrec
  def noBackRefs(cant: Set[String], allowed: Seq[PluginDeets]): Seq[PluginDeets] = {
    val (stillAllowed, nomore) = allowed.partition {
      case PluginDeets(_, pId, _, _, _) => depMap.get(pId).forall(!_.exists(cant))
    }
    if (nomore.isEmpty) stillAllowed else noBackRefs(cant ++ nomore.map(_.pId), stillAllowed)
  }

  val allowed2 = noBackRefs(others.map(_.pId).toSet, allowed)
  val allowedIds = allowed2.map(_.pId).toSet
  val imp_exts = allowed2.map {
    case PluginDeets(bd, pId, e, imps, _) =>
      val exts = e.getChildren("extension").asScala.toList.map(e => (bd, pId, e.detach()))
      (imps, exts, bd, pId)
  }
  val imports = imp_exts.map(_._1).reduce(_ ++ _)
  val exts = imp_exts.map(_._2).reduce(_ ++ _)

  val mergedPId = "com.equella.core"
  val plugElem = new Element("plugin")
  plugElem.setAttribute("id", mergedPId)
  plugElem.setAttribute("version", "1")

  val sortedImports = imports.map(e => (e.getAttributeValue("plugin-id"), e))
    .filterNot(v => allowedIds(v._1))
    .toMap.values.toSeq.sortBy(_.getAttributeValue("plugin-id"))
  val req = new Element("requires")
  req.addContent(sortedImports.map(_.clone).asJava)
  plugElem.addContent(req)

  val guiceExt = new Element("extension")
  guiceExt.setAttribute("plugin-id", "com.tle.core.guice")
  guiceExt.setAttribute("point-id", "module")
  guiceExt.setAttribute("id", "guiceModules")

  val guiceModules = exts.flatMap {
    case (bd, pId, e) => e.getAttributeValue("plugin-id") match {
      case "com.tle.core.guice" =>
        e.getChildren("parameter").asScala.map(_.getAttributeValue("value"))
      case _ => Seq.empty
    }
  }
  val langStrings = exts.flatMap {
    case (bd, pId, e) => (e.getAttributeValue("plugin-id"), e.getAttributeValue("point-id")) match {
      case ("com.tle.common.i18n", "bundle") =>
        val params = e.getChildren("parameter").asScala
        params.find(_.getAttributeValue("id") == "file").map { fileElem =>
          val filename = fileElem.getAttributeValue("value")
          val group = params.find(_.getAttributeValue("id") == "group").map(_.getAttributeValue("value")).getOrElse("resource-centre")
          val propFile = bd / "resources" / filename
          val langProps = new Properties()
          Using.fileInputStream(propFile) { inp =>
            if (IO.split(filename)._2 == "xml") langProps.loadFromXML(inp) else langProps.load(inp)
          }
          langProps.entrySet().asScala.toSeq.map {
            e => LangString(group, e.getKey.toString, pId, e.getValue.toString)
          }
        }.getOrElse(Seq.empty)
      case _ => Seq.empty
    }
  }

  val bundles = langStrings.groupBy(_.group).map {
    case (g, strings) =>
      val props = new OrderedProperties
      strings.foreach { ls => props.put(ls.key, ls.value)}
      val fname = s"lang/i18n-$g.properties"
      IO.write(props, g, baseRes / fname)
      val bundleExt = new Element("extension")
      bundleExt.setAttribute("plugin-id", "com.tle.common.i18n")
      bundleExt.setAttribute("point-id", "bundle")
      bundleExt.setAttribute("id", s"strings_$g")
      val groupE = new Element("parameter")
      groupE.setAttribute("id", "group")
      groupE.setAttribute("value", g)
      val fileE = new Element("parameter")
      fileE.setAttribute("id", "file")
      fileE.setAttribute("value", fname)
      bundleExt.addContent(groupE)
      bundleExt.addContent(fileE)
      bundleExt
  }

  def reprefix(pId: String, e: Element): Element = {
    e.getChildren("parameter").asScala.filter(p => Option(p.getAttributeValue("id")).exists(_.endsWith("Key"))).foreach {
      p => p.setAttribute("value", mergedPId + p.getAttributeValue("value").substring(pId.length))
    }
    e
  }

  val afterExt = exts.flatMap {
    case (bd, pId, e) => (e.getAttributeValue("plugin-id"), e.getAttributeValue("point-id")) match {
      case ("com.tle.core.guice", _) => Seq.empty
      case ("com.tle.common.i18n", "bundle") => Seq.empty
      case (_, "portletRenderer"|"resourceViewer"|"connectorType"|"portletType") => Seq(reprefix(pId, e.clone))
      case _ => Seq(e)
    }
  }
  guiceModules.distinct.sorted.foreach { m =>
    val e = new Element("parameter")
    e.setAttribute("id", "class")
    e.setAttribute("value", m)
    guiceExt.addContent(e)
  }
  plugElem.addContent(guiceExt)

  def containsId(el: List[Element])(e: Element) = el.exists(e2 => e2.getAttributeValue("id") == e.getAttributeValue("id"))

  plugElem.addContent(Uniqueify.uniqueSeq[Element]((i, e) =>
    e.clone().setAttribute("id", e.getAttributeValue("id") + "_" + i), containsId)(afterExt ++ bundles).asJava)

  val doc = new Document()
  doc.setDocType(new DocType("plugin", "-//JPF//Java Plug-in Manifest 1.0", "http://jpf.sourceforge.net/plugin_1_0.dtd"))
  doc.setRootElement(plugElem)
  val pluginJpf = new XMLOutputter(Format.getPrettyFormat).outputString(doc)
  IO.write(basePlugin / "plugin-jpf2.xml", pluginJpf)
  val pathsTo = imp_exts.flatMap { i =>
    val bd = i._3
    val pId = i._4
    IO.copyDirectory(bd / "src", baseSrc, overwrite = false)
    IO.copyDirectory(bd / "scalasrc", baseScalaSrc, overwrite = false)
    val plugRes = bd / "resources"
    val allRes = (plugRes ***) --- (plugRes * "lang" ***)
    val relative = allRes.pair(rebase(plugRes, "")).collect {
      case (f, p) if f.isFile => (p, pId, f.length())
    }
    val mapped = allRes.pair(rebase(plugRes, baseRes))
    IO.copy(mapped, overwrite = false)
    // IO.delete(bd)
    relative
  }
  pathsTo.groupBy(_._1).filter(t => t._2.map(_._3).distinct.size > 1).foreach {
    case (p, pids) => println(s"$p=${pids.map(t => t._2 -> t._3)}")
  }
  langStrings.groupBy { case LangString(g,k, _,_) => (g,k) }.filter(_._2.map(_.value).distinct.size > 1).toSeq.sortBy(_._1).foreach {
    case (k, dupes) => println(s"$k=${dupes.map(_.pluginId).mkString(",")}")
  }
  exts.flatMap {
    case (bd, pId, e) => e.getChildren("parameter").asScala.collect {
      case p if Option(p.getAttributeValue("value")).exists(_.startsWith(pId))
        && Option(p.getAttributeValue("id")).exists(_.endsWith("Key"))
      => (e.getAttributeValue("plugin-id"), e.getAttributeValue("point-id"), p.getAttributeValue("id"), p.getAttributeValue("value"))
    }
  }.foreach(println)
}
