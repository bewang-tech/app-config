package com.napster.bi.config

import java.util.concurrent.TimeUnit

import com.typesafe.config.{Config, ConfigValueType}

import scala.language.dynamics
import scala.collection.JavaConverters._

class AppConfigException(msg: String) extends Exception(msg)

object AppConfig {

  def apply(name: String, config: Config) = Root(name, config.getConfig(name))

  case class Value(name: String, parent: ConfigObject) extends AppConfig {
    override def apply(field: String) = throw new AppConfigException(s"$path is a value, not an object")

    override lazy val int = parent.config.getInt(name)
    override lazy val string = parent.config.getString(name)
    override lazy val long = parent.config.getLong(name)
    override lazy val bool = parent.config.getBoolean(name)
    override lazy val size = parent.config.getBytes(name).longValue()

    override def duration(unit: TimeUnit): Long = parent.config.getDuration(name, unit)

    override lazy val strings = parent.config.getStringList(name).asScala
  }

//  case class Array(name: String, parent: Node) extends AppConfig {
//    def seq[T: TypeTag] =
//  }

  trait ConfigObject extends AppConfig {
    def getField(field: String): AppConfig =
      if (config.hasPath(field)) {
        val valueType = config.getValue(field).valueType()
        valueType match {
          case ConfigValueType.OBJECT => new Node(field, this, config.getConfig(field))
          // case ConfigValueType.LIST => new Array(field, this)
          case _ => new Value(field, this)
        }
      } else NonExistentValue(field, this)
  }

  case class Node(name: String, parent: AppConfig, override val config: Config) extends ConfigObject {
    private[this] val fields = collection.mutable.Map[String, AppConfig]()

    override def apply(field: String) = fields.getOrElseUpdate(field, getField(field))
  }

  case class Root(name: String, override val config: Config) extends ConfigObject {

    override def parent = throw new AppConfigException(s"$name is a root.")

    override def apply(field: String) = getField(field)
  }

  case class NonExistentValue(name: String, parent: AppConfig) extends AppConfig {

    override def apply(field: String) = error

  }

}

trait AppConfig extends Dynamic {
  def config: Config = error

  def int: Int = error
  def string: String = error
  def long: Long = error
  def bool: Boolean = error
  def duration(unit: TimeUnit): Long = error
  def size: Long = error

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