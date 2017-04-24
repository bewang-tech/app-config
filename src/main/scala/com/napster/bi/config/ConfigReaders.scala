package com.napster.bi.config

import com.typesafe.config.Config

object ConfigReaders {

  type ConfigReader[T] = Function2[String, Config, T]

  implicit val intReader: ConfigReader[Int] = (path, config) => config.getInt(path)
  implicit val stringReader: ConfigReader[String] = (path, config) => config.getString(path)
  implicit val boolReader: ConfigReader[Boolean] = (path, config) => config.getBoolean(path)
  implicit val longReader: ConfigReader[Long] = (path, config) => config.getLong(path)
  implicit val doubleReader: ConfigReader[Double] = (path, config) => config.getDouble(path)
  implicit val floatReader: ConfigReader[Float] = (path, config) => config.getDouble(path).toFloat


}
