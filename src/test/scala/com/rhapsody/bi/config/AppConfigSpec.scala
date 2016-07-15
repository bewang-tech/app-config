package com.rhapsody.bi.config

import org.scalatest._
import com.typesafe.config.ConfigFactory
import collection.JavaConversions._
import AppConfig._
import java.util.concurrent.TimeUnit

class AppConfigSpec extends WordSpec with Matchers {

  "AppConfig" should {

    def appConf(config: String, rootName: String = "my-app") = {
      AppConfig(rootName, ConfigFactory.parseString(config))
    }

    "get a config value" when {
      "the value is String" in {
        appConf("my-app { country = US }").country.string should be("US")
      }
      "the value is Int" in {
        appConf("my-app { age = 23 }").age.int should be(23)
      }
      "the value is Long" in {
        appConf("my-app { population = 15 }").population.long should be(15L)
      }
      "the value is Boolean" in {
        appConf("my-app { isMale = false }").isMale.bool should be(false)
      }
      "the value is Duration" in {
        appConf("my-app { interval = 5m }").interval.duration(TimeUnit.SECONDS) should be(300)
      }
      "the value is Size" in {
        appConf("my-app { maxSize = 5m }").maxSize.size should be(5 * 1024 * 1024)
      }
      "the value is a list of strings" in {
        val result = appConf("my-app { topics = [ topic-1, topic-2 ] }").topics.strings
        result should contain allOf("topic-1", "topic-2")
      }
      "the key is not valid identifier" in {
        appConf("my-app { is-male = false }")("is-male").bool should be(false)
      }

    }

    "get a nested config value" when {
      "the value is String" in {
        appConf(
          """my-app {
          country {
            US {
              WA {
                region = NW
              }
            }
          }
        }""").country.US.WA.region.string should be("NW")
      }
      "the value is Int" in {
        appConf(
          """my-app {
          year {
            artists {
              join.task {
                nparts = 15
              }
            }
          }
        }""").year.artists.join.task.nparts.int should be(15)
      }
      "the value is Long" in {
        appConf(
          """my-app {
          country {
            US {
              WA {
                population = 15
              }
            }
          }
        }""").country.US.WA.population.long should be(15L)
      }
      "the value is Boolean" in {
        appConf(
          """my-app {
          year {
            artists {
              recompute = false
            }
          }
        }""").year.artists.recompute.bool should be(false)
      }
    }

    "return NonExistentValue" when {
      val nonExistentKey = "non-existent-key"
      "the field name doesn't exist" in {
        val conf = appConf("my-app {}")
        conf(nonExistentKey) should be(NonExistentValue(nonExistentKey, conf))
      }
      "the nested field doesn't exist" in {
        an[AppConfigException] should be thrownBy
          appConf(
            """my-app {
            country {
              US {
                north-west {
                  WA = 1
                }
              }
            }
          }""").country.US("north-west").FL.int
      }
    }

    "get the path" when {
      "there is only one level" in {
        appConf("my-app { country = US }").country.path should be("my-app.country")
      }
      "the config is nested" in {
        appConf(
          """my-app {
          year {
            artists {
              join.task {
                nparts = 15
              }
            }
          }
        }""").year.artists.join.task.nparts.path should be("my-app.year.artists.join.task.nparts")
      }
    }

//    "get an array" when {
//      "element type is string" in {
//        appConf(
//          """
//            |my-app {
//            |  list = [ "a", "b" ]
//            |}
//          """.stripMargin).list.seq[String] should be(Seq("a", "b"))
//      }
//    }

  }
}