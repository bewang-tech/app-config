package com.napster.bi.config

import com.typesafe.config.ConfigFactory
import org.scalatest._

class AppConfigSpec extends WordSpec with Matchers {

  "AppConfig" should {

    def appConf(config: String, rootName: String = "my-app") = {
      AppConfig(rootName, ConfigFactory.parseString(config))
    }

    import AppConfig._

    "get a primitive type config value" which {
      "is a Boolean value" when {
        val required = appConf("my-app { required = true }").required
        "using .as[Boolean]" in {
          required.as[Boolean] should be(true)
        }
        "using .bool" in {
          required.bool should be(true)
        }
        "passing to a function" in {
          def f(in: Boolean) = in

          f(required) should be(true)
        }
      }
      "is an Int value" when {
        val age = appConf("my-app { age = 16 }").age
        "using .as[Int]" in {
          age.as[Int] should be(16)
        }
        "using .int" in {
          age.int should be(16)
        }
        "passing to a function" in {
          def f(in: Int) = in

          f(age) should be(16)
        }
      }
      "is a String value" when {
        val country = appConf("my-app { country = US }").country
        "using .as[String]" in {
          country.as[String] should be("US")
        }
        "using .string" in {
          country.string should be("US")
        }
        "passing to a function" in {
          def f(in: String) = in

          f(country) should be("US")
        }
      }
      "is an Long value" when {
        val count = appConf("my-app { count = 9999 }").count
        "using .as[Long]" in {
          count.as[Long] should be(9999L)
        }
        "using .long" in {
          count.long should be(9999L)
        }
        "passing to a function" in {
          def f(in: Long) = in

          f(count) should be(9999L)
        }
      }
    }
  }
}