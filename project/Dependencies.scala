import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Dependencies {


  val scalajsdom  = Def.setting {
    Seq("org.scala-js" %%% "scalajs-dom" % DependencyVersions.scalajsdom)
  }
  val scalatest   :     Def.Initialize[Seq[ModuleID]] = Def.setting {
    Seq(
      "org.scalactic" %%% "scalactic"  % DependencyVersions.scalatest,
      "org.scalatest" %%% "scalatest" % DependencyVersions.scalatest % "test"
    )
  }


  val laminar: Def.Initialize[Seq[ModuleID]] = Def.setting {
    Seq(
      "com.raquo" %%% "laminar" % DependencyVersions.laminar
    )
  }

  val upickle: Def.Initialize[Seq[ModuleID]] = Def.setting {
    Seq(
      "com.lihaoyi" %%% "upickle" % DependencyVersions.`upickle`
    )
  }


  val cats = Def.setting {
    Seq(
       "org.typelevel" %%% "cats-core" % DependencyVersions.cats,
        "org.typelevel" %%% "cats-laws" % DependencyVersions.cats % Test,
      // For property-based testing
      "org.scalacheck" %% "scalacheck" % DependencyVersions.scalacheck % Test,
      // The testing framework
      "org.scalameta" %% "munit" % DependencyVersions.munit % Test,
      // Discipline connects laws to the test framework
      "org.typelevel" %% "discipline-munit" % DependencyVersions.discipline_munit % Test

    )
  }


}
