package com.napster.bi.config

import com.typesafe.config.{ConfigException, ConfigFactory}
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
      "is an Float value" when {
        val value: Float = 0.123456789f
        val weight = appConf(s"my-app { weight = $value }").weight

        "using .as[Float]" in {
          weight.as[Float] should be(value)
        }
        "using .float" in {
          weight.float should be(value)
        }
        "passing to a function" in {
          def f(in: Float) = in

          f(weight) should be(value)
        }
      }
      "is an Double value" when {
        val value = 0.123456789d
        val weight = appConf(s"my-app { weight = $value }").weight

        "using .as[Double]" in {
          weight.as[Double] should be(value)
        }
        "using .double" in {
          weight.double should be(value)
        }
        "passing to a function" in {
          def f(in: Double) = in

          f(weight) should be(value)
        }
      }
    }

    "return None" when {
      "the path doesn't exist" when {
        "only one level" in {
          appConf("my-app { }").nonExistValue.asOption[Int] should be(None)
        }
        "multiple levels" in {
          appConf("my-app { }").nonExistLevel1.nonExistLevel2.nonExistValue.asOption[Int] should be(None)
        }
        "a level is a value, not an object" in {
          appConf("my-app { count = 15 }").count.nonExistLevel2.nonExistValue.asOption[Int] should be(None)
        }
      }
    }
    "return Some(value)" when {
      "the path exists" in {
        appConf("my-app { age: 18 }").age.asOption[Int] should be(Some(18))
      }
    }

    "read a list of values" when {
      "a list of ints" in {
        appConf("my-app { ages = [ 13, 15, 17, 19 ] }").ages.as[Seq[Int]] should be(
          Seq(13, 15, 17, 19)
        )
      }
      "a list of strings" in {
        appConf("my-app { hosts = [ host1, host2, host3 ] }").hosts.as[Seq[String]] should be(
          Seq("host1", "host2", "host3")
        )
      }
    }

    "raise exception" when {
      "reading as wrong type" when {
        "reading a list from a config" in {
          a[ConfigException] should be thrownBy {
            appConf("my-app { settings { baseDir = /data/my-app} }").settings.as[Seq[String]]
          }
        }
        "reading a list from a value" in {
          a[ConfigException] should be thrownBy {
            appConf("my-app { host = napster.com }").host.as[Seq[String]]
          }
        }
      }
    }
  }

}
