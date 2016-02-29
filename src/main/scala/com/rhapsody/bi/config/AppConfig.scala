package com.rhapsody.bi.config

import java.util.concurrent.TimeUnit

import scala.collection.JavaConverters._
import scala.language.dynamics

import com.typesafe.config.Config
import com.typesafe.config.ConfigValueType

class AppConfigException(msg: String) extends Exception(msg)

trait AppConfig extends Dynamic {
  def config: Config = error
  def int: Int = error
  def string: String = error
  def long: Long = error
  def bool: Boolean = error
  def duration(unit: TimeUnit): Long = error
  
  def strings: Seq[String] = error

  def parent: AppConfig
  def name: String

  def apply(field: String): AppConfig

  def applyDynamic(field: String)(subField: String) = {
    apply(field).apply(subField)
  }

  def selectDynamic(field: String) = apply(field)

  def path: String = {
    val parentPath = parent match {
      case AppConfig.Root(pname, _) => pname
      case _ => parent.path
    }
    parentPath + "." + name
  }

  def error() = throw new AppConfigException(s"$path doesn't exist.")
}

object AppConfig {

  def apply(name: String, config: Config) = Root(name, config.getConfig(name))

  case class Value(name: String, parent: AppConfig) extends AppConfig {
    override def apply(field: String) = throw new AppConfigException(s"$path is a value, not an object")

    override lazy val int = parent.config.getInt(name)
    override lazy val string = parent.config.getString(name)
    override lazy val long = parent.config.getLong(name)
    override lazy val bool = parent.config.getBoolean(name)

    override def duration(unit: TimeUnit): Long = parent.config.getDuration(name, unit)
    
    override lazy val strings = parent.config.getStringList(name).asScala
  }

  trait ConfigObject extends AppConfig {

    def createField(field: String): AppConfig =
      if (config.hasPath(field)) {
        val valueType = config.getValue(field).valueType()
        valueType match {
          case ConfigValueType.OBJECT => new Node(field, this, config.getConfig(field))
          case _ => new Value(field, this)
        }
      } else NonExistentValue(field, this)

  }

  case class Node(name: String, parent: AppConfig, override val config: Config) extends ConfigObject {
    private[this] val fields = collection.mutable.Map[String, AppConfig]()

    override def apply(field: String) =
      fields.getOrElseUpdate(field, createField(field))
  }

  case class Root(name: String, override val config: Config) extends ConfigObject {

    override def parent = throw new AppConfigException(s"$name is a root.")

    override def apply(field: String) = createField(field)

  }

  case class NonExistentValue(name: String, override val parent: AppConfig) extends AppConfig {

    override def apply(field: String) = error

  }

}