package com.napster.bi.config

import com.typesafe.config.{ConfigFactory, ConfigValueType}
import org.scalatest.{Matchers, WordSpec}

class ConfigSpec extends WordSpec with Matchers {

  import ConfigFactory._

  "Config" should {
    "return a Config for a value" in {
      val conf = parseString("my-app { country = US }")
      val myApp = conf.getConfig("my-app")
      val vMyApp = conf.getValue("my-app")
      vMyApp.valueType() should be (ConfigValueType.OBJECT)
      val vCountry = myApp.getValue("country")
      vCountry.valueType() should not be (ConfigValueType.OBJECT)
      // val country = conf.getConfig("country")
      myApp.hasPath("country") should be (true)
    }
    "a value is a list" in {
      val conf = parseString("my-app { countries = [ US, {UK = SMALL}, CN ] }")
      val vCountries = conf.getValue("my-app.countries")
      vCountries.valueType() should be (ConfigValueType.LIST)
    }
  }

}
