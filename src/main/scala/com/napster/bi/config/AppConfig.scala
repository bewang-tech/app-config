package com.napster.bi.config

import com.typesafe.config.{Config, ConfigValueType}

import scala.collection.JavaConversions._
import scala.language.{dynamics, implicitConversions}

class AppConfigException(msg: String) extends Exception(msg)

trait AppConfig

object AppConfig {

  def apply(name: String, config: Config): Node = Container(name, "", config)

  type ConfigReader[T] = Function2[String, Config, T]

  implicit val intReader: ConfigReader[Int] = (path, config) => config.getInt(path)
  implicit val stringReader: ConfigReader[String] = (path, config) => config.getString(path)
  implicit val boolReader: ConfigReader[Boolean] = (path, config) => config.getBoolean(path)
  implicit val longReader: ConfigReader[Long] = (path, config) => config.getLong(path)
  implicit val doubleReader: ConfigReader[Double] = (path, config) => config.getDouble(path)
  implicit val floatReader: ConfigReader[Float] = (path, config) => config.getDouble(path).toFloat

  implicit val configReader: ConfigReader[Config] = (path, config) => config.getConfig(path)

  implicit val intSeqReader: ConfigReader[Seq[Int]] = (path, config) => config.getIntList(path).map(_.intValue)
  implicit val stringSeqReader: ConfigReader[Seq[String]] = (path, config) => config.getStringList(path)
  implicit val boolSeqReader: ConfigReader[Seq[Boolean]] = (path, config) => config.getBooleanList(path).map(_.booleanValue())
  implicit val longSeqReader: ConfigReader[Seq[Long]] = (path, config) => config.getLongList(path).map(_.longValue())
  implicit val doubleSeqReader: ConfigReader[Seq[Double]] = (path, config) => config.getDoubleList(path).map(_.doubleValue())
  implicit val floatSeqReader: ConfigReader[Seq[Float]] = (path, config) => config.getDoubleList(path).map(_.toFloat)

  trait Node extends AppConfig with Dynamic {

    def apply(key: String): Node

    def selectDynamic(key: String): Node = apply(key)

    def as[T: ConfigReader]: T

    def asOption[T: ConfigReader]: Option[T]

  }

  case class Value(name: String, path: String, parent: Config) extends Node {

    override def as[T: ConfigReader]: T = {
      val reader = implicitly[ConfigReader[T]]
      reader(name, parent)
    }

    override def asOption[T: ConfigReader]: Option[T] =
      Some(as[T])

    override def apply(key: String) =
      NonExistNode(key, s"$path.$key", parent)

  }

  case class Container(name: String, path: String, parent: Config) extends Node {
    private[this] val self = parent.getConfig(name)

    private[this] val cache = collection.mutable.Map[String, Node]()

    private def isObject(key: String) =
      self.getValue(key).valueType() == ConfigValueType.OBJECT

    override def as[T: ConfigReader]: T = {
      val reader = implicitly[ConfigReader[T]]
      reader(name, parent)
    }

    override def asOption[T: ConfigReader]: Option[T] =
      Some(as[T])

    private def create(key: String, childPath: String) =
      if (isObject(key))
        Container(key, childPath, self)
      else
        Value(key, childPath, self)

    override def apply(key: String): Node = {
      val childPath = s"$path.$key"
      if (self.hasPath(key))
        cache.getOrElseUpdate(key, create(key, childPath))
      else
        NonExistNode(key, childPath, self)
    }
  }

  case class NonExistNode(name: String, path: String, parent: Config) extends Node {
    override def as[T: ConfigReader]: T =
      throw new NoSuchElementException(s"$path doesn't exist")

    override def asOption[T: ConfigReader]: Option[T] = None

    override def apply(key: String) =
      NonExistNode(key, s"$path.$key", parent)
  }

  implicit class Node2TypedValue(nd: Node) {
    def int: Int = nd.as[Int]

    def string: String = nd.as[String]

    def bool: Boolean = nd.as[Boolean]

    def long: Long = nd.as[Long]

    def double: Double = nd.as[Double]

    def float: Float = nd.as[Float]
  }

  implicit def node2Int(nd: Node): Int = nd.as[Int]

  implicit def node2String(nd: Node): String = nd.as[String]

  implicit def node2Boolean(nd: Node): Boolean = nd.as[Boolean]

  implicit def node2Long(nd: Node): Long = nd.as[Long]

  implicit def node2Double(nd: Node): Double = nd.as[Double]

  implicit def node2Float(nd: Node): Float = nd.as[Float]

}
