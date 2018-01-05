import java.util.Properties

import org.jdom2.filter.{AbstractFilter, Filters}
import org.jdom2.output.{Format, XMLOutputter}
import org.jdom2.{DocType, Document, Element}
import sbt.Keys.Classpath
import sbt._

import scala.annotation.tailrec
import scala.collection.JavaConverters._

class ElementFilter(f: Element => Boolean) extends AbstractFilter[Element] {
  override def filter(content: scala.Any): Element = content match {
    case e: Element if f(e) => e
    case _ => null
  }
}

case class PluginDeets(bd: File, libs: Classpath) {
  lazy val rootDoc = Common.saxBuilder.build(bd / "plugin-jpf.xml")
  lazy val rootElem = rootDoc.getRootElement
  lazy val imports = Option(rootElem.getChild("requires")).toSeq.flatMap(_.getChildren("import").asScala)
    .filter(!_.getAttributeValue("plugin-id").contains(":"))
  lazy val importIds = imports.map(_.getAttributeValue("plugin-id"))
  lazy val pId = rootElem.getAttributeValue("id")
}

case class LangString(group: String, key: String, pluginId: String, value: String)

object PluginRefactor {

  def getPluginId(e: Element) : String = e.getAttributeValue("plugin-id")

  def choosePlugins(allImports: Seq[PluginDeets]): Iterable[String] = {

    val platformPlugins = Set("com.tle.platform.common", "com.tle.platform.swing", "com.tle.platform.equella")

    val keepPlugins = Set("com.tle.log4j", "com.tle.webstart.admin", "com.tle.web.adminconsole", "com.equella.core")++platformPlugins
    val allowedImports = Set("org.codehaus.jackson","org.hibernate")++platformPlugins

    val needReason = Set("com.tle.common.item", "com.tle.common.collection")

    @tailrec
    def minimalPlugins(allowedMap: Map[String, PluginDeets]): Map[String, PluginDeets] = {
      allowedMap.values.find(_.importIds.exists(i => !allowedMap.contains(i) && !allowedImports(i))) match {
        case Some(illegal) =>
          if (illegal.pId.contains("common")) {
            val reason = illegal.importIds.filterNot(allowedMap.keySet)
            println(s"${illegal.pId} evicted because of ${reason.mkString(",")}")
          }
          minimalPlugins(allowedMap - illegal.pId)
        case None => allowedMap
      }
    }

    val depMap = allImports.flatMap {
      p => p.importIds.map(_ -> p.pId)
    }.groupBy(_._1).mapValues(_.map(_._2))

    val initialPlugins = allImports.filter { p =>
      val r = p.rootElem
      val adminConsole = r.getChildren("attributes").asScala.flatMap(_.getChildren("attribute").asScala).find {
        _.getAttributeValue("id") == "type"
      }.exists(_.getAttributeValue("value") == "admin-console")

      p.libs.isEmpty && !adminConsole && r.getChildren("extension-point").isEmpty &&
        // r.getChildren("extension").isEmpty &&
        !keepPlugins(p.pId) && !(p.bd / "build.sbt").exists
    }

    minimalPlugins(initialPlugins.map(p => (p.pId, p)).toMap).values.map(_.pId)
  }


  def mergePlugins(allBaseDirs: Seq[(File, Classpath)],
                   baseDir: File, pluginId: String, modify: Boolean) : Unit = {
    val allPlugins = allBaseDirs.map(t => PluginDeets(t._1, t._2))
    val toMerge = choosePlugins(allPlugins)

    println("Merging: "+ toMerge.toSeq.sorted.mkString("\n"))
    IO.delete(baseDir)

    val baseSrc = baseDir / "src"
    val baseScalaSrc = baseDir / "scalasrc"
    val baseTestSrc = baseDir / "test"
    val baseRes = baseDir / "resources"

    val allowedIds = toMerge.toSet
    val imp_exts = allPlugins.collect {
      case p if allowedIds(p.pId) =>
        val exts = p.rootElem.getChildren("extension").asScala.toList.map(e => (p.bd, p.pId, e.detach()))
        (p.imports, exts, p.bd, p.pId)
    }

    val imports = imp_exts.map(_._1).reduce(_ ++ _)
    val exts = imp_exts.map(_._2).reduce(_ ++ _)

    val plugElem = new Element("plugin")
    plugElem.setAttribute("id", pluginId)
    plugElem.setAttribute("version", "1")

    val sortedImports = imports.map(e => (getPluginId(e), e))
      .filterNot(v => allowedIds(v._1))
      .toMap.values.toSeq.sortBy(getPluginId)
    val req = new Element("requires")
    req.addContent(sortedImports.map(_.clone).asJava)
    plugElem.addContent(req)

    val guiceExt = new Element("extension")
    guiceExt.setAttribute("plugin-id", "com.tle.core.guice")
    guiceExt.setAttribute("point-id", "module")
    guiceExt.setAttribute("id", "guiceModules")

    val guiceModules = exts.flatMap {
      case (bd, pId, e) => getPluginId(e) match {
        case "com.tle.core.guice" =>
          e.getChildren("parameter").asScala.map(_.getAttributeValue("value"))
        case _ => Seq.empty
      }
    }
    val langStrings = exts.flatMap {
      case (bd, pId, e) => (getPluginId(e), e.getAttributeValue("point-id")) match {
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
        p => p.setAttribute("value", pluginId + p.getAttributeValue("value").substring(pId.length))
      }
      e
    }

    val afterExt = exts.flatMap {
      case (bd, pId, e) => (getPluginId(e), e.getAttributeValue("point-id")) match {
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

    def containsId(el: List[Element])(e: Element) =
      el.exists(e2 => e2.getAttributeValue("id") == e.getAttributeValue("id"))

    plugElem.addContent(Uniqueify.uniqueSeq[Element]((i, e) =>
      e.clone().setAttribute("id", e.getAttributeValue("id") + "_" + i), containsId)(afterExt ++ bundles).asJava)

    val doc = new Document()
    doc.setDocType(new DocType("plugin", "-//JPF//Java Plug-in Manifest 1.0",
      "http://jpf.sourceforge.net/plugin_1_0.dtd"))
    doc.setRootElement(plugElem)

    val pluginJpf = new XMLOutputter(Format.getPrettyFormat).outputString(doc)
    val manifestName = if (modify) "plugin-jpf.xml" else "plugin-jpf2.xml"
    IO.write(baseDir / manifestName, pluginJpf)
    val pathsTo = imp_exts.flatMap { i =>
      val bd = i._3
      val pId = i._4
      IO.copyDirectory(bd / "src", baseSrc, overwrite = false)
      IO.copyDirectory(bd / "scalasrc", baseScalaSrc, overwrite = false)
      IO.copyDirectory(bd / "test", baseTestSrc, overwrite = false)
      val plugRes = bd / "resources"
      val allRes = (plugRes ***) --- (plugRes * "lang" ***)
      val relative = allRes.pair(rebase(plugRes, "")).collect {
        case (f, p) if f.isFile => (p, pId, f.length())
      }
      val mapped = allRes.pair(rebase(plugRes, baseRes))
      IO.copy(mapped, overwrite = false)
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

    val needsReplacing = new ElementFilter( {
      e => allowedIds(getPluginId(e))
    })
    val hasOldStyle = new ElementFilter( {
      e => getPluginId(e).contains(":")
    })
    allPlugins.foreach {
      case p if !allowedIds(p.pId) => {
        Option(p.rootElem.getChild("requires")).foreach { r =>
          val removed = r.removeContent[Element](needsReplacing)
          val removedOld = r.removeContent[Element](hasOldStyle)
          if (!removed.isEmpty) {
            val impElem = new Element("import")
            impElem.setAttribute("plugin-id", pluginId)
            impElem.setAttribute("exported", "true")
            r.addContent(impElem)
          }
          val allRemoved = (removed.asScala ++ removedOld.asScala).map(getPluginId)
          if (allRemoved.nonEmpty) println(s"Removing from ${p.pId}: ${allRemoved.mkString(",")}")
        }
        val newManifest = new XMLOutputter(Format.getPrettyFormat).outputString(p.rootDoc)
        if (modify) IO.write(p.bd / "plugin-jpf.xml", newManifest)
      }
      case p => if (modify) IO.delete(p.bd)
    }

  }
}
