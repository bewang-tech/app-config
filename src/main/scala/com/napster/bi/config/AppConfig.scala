package com.napster.bi.config

import com.typesafe.config.{Config, ConfigValueType}

import scala.collection.JavaConversions._
import scala.language.{dynamics, implicitConversions}

class AppConfigException(msg: String) extends Exception(msg)

trait AppConfig extends Dynamic {

  def apply(key: String): AppConfig

  def selectDynamic(key: String): AppConfig = apply(key)

  def as[T: AppConfig.Read]: T

  def asOption[T: AppConfig.Read]: Option[T]

}

object AppConfig {

  def apply(name: String, config: Config): AppConfig = Container(name, "", config)

  type Read[T] = Function2[String, Config, T]

  implicit val intReader: Read[Int] = (path, config) => config.getInt(path)
  implicit val stringReader: Read[String] = (path, config) => config.getString(path)
  implicit val boolReader: Read[Boolean] = (path, config) => config.getBoolean(path)
  implicit val longReader: Read[Long] = (path, config) => config.getLong(path)
  implicit val doubleReader: Read[Double] = (path, config) => config.getDouble(path)
  implicit val floatReader: Read[Float] = (path, config) => config.getDouble(path).toFloat

  implicit val configReader: Read[Config] = (path, config) => config.getConfig(path)

  implicit val intSeqReader: Read[Seq[Int]] = (path, config) => config.getIntList(path).map(_.intValue)
  implicit val stringSeqReader: Read[Seq[String]] = (path, config) => config.getStringList(path)
  implicit val boolSeqReader: Read[Seq[Boolean]] = (path, config) => config.getBooleanList(path).map(_.booleanValue())
  implicit val longSeqReader: Read[Seq[Long]] = (path, config) => config.getLongList(path).map(_.longValue())
  implicit val doubleSeqReader: Read[Seq[Double]] = (path, config) => config.getDoubleList(path).map(_.doubleValue())
  implicit val floatSeqReader: Read[Seq[Float]] = (path, config) => config.getDoubleList(path).map(_.toFloat)

  case class Value(name: String, path: String, parent: Config) extends AppConfig {

    override def as[T: Read]: T = {
      val read = implicitly[Read[T]]
      read(name, parent)
    }

    override def asOption[T: Read]: Option[T] =
      Some(as[T])

    override def apply(key: String) =
      NonExistAppConfig(key, s"$path.$key", parent)

  }

  case class Container(name: String, path: String, parent: Config) extends AppConfig {
    private[this] val self = parent.getConfig(name)

    private[this] val cache = collection.mutable.Map[String, AppConfig]()

    private def isObject(key: String) =
      self.getValue(key).valueType() == ConfigValueType.OBJECT

    override def as[T: Read]: T = {
      val read = implicitly[Read[T]]
      read(name, parent)
    }

    override def asOption[T: Read]: Option[T] =
      Some(as[T])

    private def create(key: String, childPath: String) =
      if (isObject(key))
        Container(key, childPath, self)
      else
        Value(key, childPath, self)

    override def apply(key: String): AppConfig = {
      val childPath = s"$path.$key"
      if (self.hasPath(key))
        cache.getOrElseUpdate(key, create(key, childPath))
      else
        NonExistAppConfig(key, childPath, self)
    }
  }

  case class NonExistAppConfig(name: String, path: String, parent: Config) extends AppConfig {
    override def as[T: Read]: T =
      throw new NoSuchElementException(s"$path doesn't exist")

    override def asOption[T: Read]: Option[T] = None

    override def apply(key: String) =
      NonExistAppConfig(key, s"$path.$key", parent)
  }

  implicit class AppConfig2TypedValue(nd: AppConfig) {
    def int: Int = nd.as[Int]

    def string: String = nd.as[String]

    def bool: Boolean = nd.as[Boolean]

    def long: Long = nd.as[Long]

    def double: Double = nd.as[Double]

    def float: Float = nd.as[Float]
  }

  implicit def node2Int(nd: AppConfig): Int = nd.as[Int]

  implicit def node2String(nd: AppConfig): String = nd.as[String]

  implicit def node2Boolean(nd: AppConfig): Boolean = nd.as[Boolean]

  implicit def node2Long(nd: AppConfig): Long = nd.as[Long]

  implicit def node2Double(nd: AppConfig): Double = nd.as[Double]

  implicit def node2Float(nd: AppConfig): Float = nd.as[Float]

  implicit def node2Config(nd: AppConfig): Config = nd.as[Config]

}
