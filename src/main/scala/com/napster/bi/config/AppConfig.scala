package com.napster.bi.config

import com.typesafe.config.{Config, ConfigValueType}

import scala.language.{dynamics, implicitConversions}

class AppConfigException(msg: String) extends Exception(msg)

object AppConfig {

  def apply(name: String, config: Config): Node = Container(name, "", config.getConfig(name))

  import ConfigReaders._

  trait Node extends Dynamic {

    def apply(key: String): Node

    def selectDynamic(key: String): Node = apply(key)

    def as[T: ConfigReader]: T

  }

  case class Value(name: String, path: String, parent: Config) extends Node {

    override def as[T: ConfigReader]: T = {
      val reader = implicitly[ConfigReader[T]]
      reader(name, parent)
    }

    override def apply(key: String) =
      throw new NoSuchElementException(s"$path is a value which cannot have field $key.")

  }

  case class Container(name: String, path: String, self: Config) extends Node {

    private[this] val cache = collection.mutable.Map[String, Node]()

    private def isObject(key: String) =
      self.getValue(key).valueType() == ConfigValueType.OBJECT

    override def as[T: ConfigReader]: T = self.asInstanceOf[T]

    private def create(key: String) = {
      val childPath = s"$path.$key"
      if (isObject(key))
        Container(key, childPath, self.getConfig(key))
      else
        Value(key, childPath, self)
    }

    override def apply(key: String): Node =
      if (self.hasPath(key))
        cache.getOrElseUpdate(key, create(key))
      else
        throw new NoSuchElementException(s"Cannot find the key $key in $path.")
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
