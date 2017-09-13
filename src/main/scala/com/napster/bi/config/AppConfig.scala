package com.napster.bi.config

import com.typesafe.config.{Config, ConfigValueType}

import scala.collection.JavaConversions._
import scala.language.{dynamics, implicitConversions}

class AppConfigException(msg: String) extends Exception(msg)

trait AppConfig extends Dynamic {

  def apply(key: String): AppConfig

  def selectDynamic(key: String): AppConfig = apply(key)

  def as[T: AppConfig.Reader]: T

  def asOption[T: AppConfig.Reader]: Option[T]

}

object AppConfig {

  def apply(name: String, config: Config): AppConfig = Container(name, "", config)

  type Reader[T] = Function2[String, Config, T]

  implicit val intReader: Reader[Int] = (path, config) => config.getInt(path)
  implicit val stringReader: Reader[String] = (path, config) => config.getString(path)
  implicit val boolReader: Reader[Boolean] = (path, config) => config.getBoolean(path)
  implicit val longReader: Reader[Long] = (path, config) => config.getLong(path)
  implicit val doubleReader: Reader[Double] = (path, config) => config.getDouble(path)
  implicit val floatReader: Reader[Float] = (path, config) => config.getDouble(path).toFloat

  implicit val configReader: Reader[Config] = (path, config) => config.getConfig(path)

  implicit val intSeqReader: Reader[Seq[Int]] = (path, config) => config.getIntList(path).map(_.intValue)
  implicit val stringSeqReader: Reader[Seq[String]] = (path, config) => config.getStringList(path)
  implicit val boolSeqReader: Reader[Seq[Boolean]] = (path, config) => config.getBooleanList(path).map(_.booleanValue())
  implicit val longSeqReader: Reader[Seq[Long]] = (path, config) => config.getLongList(path).map(_.longValue())
  implicit val doubleSeqReader: Reader[Seq[Double]] = (path, config) => config.getDoubleList(path).map(_.doubleValue())
  implicit val floatSeqReader: Reader[Seq[Float]] = (path, config) => config.getDoubleList(path).map(_.toFloat)

  case class Value(name: String, path: String, parent: Config) extends AppConfig {

    override def as[T: Reader]: T = {
      val reader = implicitly[Reader[T]]
      reader(name, parent)
    }

    override def asOption[T: Reader]: Option[T] =
      Some(as[T])

    override def apply(key: String) =
      NonExistAppConfig(key, s"$path.$key", parent)

  }

  case class Container(name: String, path: String, parent: Config) extends AppConfig {
    private[this] val self = parent.getConfig(name)

    private[this] val cache = collection.mutable.Map[String, AppConfig]()

    private def isObject(key: String) =
      self.getValue(key).valueType() == ConfigValueType.OBJECT

    override def as[T: Reader]: T = {
      val reader = implicitly[Reader[T]]
      reader(name, parent)
    }

    override def asOption[T: Reader]: Option[T] =
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
    override def as[T: Reader]: T =
      throw new NoSuchElementException(s"$path doesn't exist")

    override def asOption[T: Reader]: Option[T] = None

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

  implicit def node2[T](nd: AppConfig): T = nd.as[T]

}
