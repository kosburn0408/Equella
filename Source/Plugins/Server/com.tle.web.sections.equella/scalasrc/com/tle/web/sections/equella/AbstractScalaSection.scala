package com.tle.web.sections.equella

import com.tle.common.Utils
import com.tle.web.sections.{Section, SectionInfo, SectionTree}

abstract class AbstractScalaSection extends Section {
  type M <: AnyRef

  var treeRegisteredIn : SectionTree = _
  var sectionId : String = _

  def getModel(info: SectionInfo): M = info.getModelForId(getSectionId)
  def newModel: SectionInfo => M

  override def instantiateModel(info: SectionInfo): AnyRef = newModel(info)

  override def getTree: SectionTree = treeRegisteredIn

  override def getSectionId: String = sectionId

  override def registered(id: String, tree: SectionTree): Unit = {
    sectionId = id
    treeRegisteredIn = tree
  }

  override def isTreeIndexed: Boolean = true

  override def treeFinished(id: String, tree: SectionTree): Unit = {}

  override def getSectionObject: Section = this

  override def getDefaultPropertyName: String = {
    val _className = getClass.getSimpleName
    val className = if (_className.endsWith("Section")) _className.substring(0, _className.length - 7) else _className
    val caps: Array[String] = className.split("[a-z0-9]*")
    caps.mkString("").toLowerCase
  }

}