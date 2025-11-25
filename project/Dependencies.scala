import org.portablescala.sbtplatformdeps.PlatformDepsPlugin.autoImport._
import sbt._

object Dependencies {


  val scalajsdom  = Def.setting {
    Seq("org.scala-js" %%% "scalajs-dom" % DependencyVersions.scalajsdom)
  }
  val scalatest   :     Def.Initialize[Seq[ModuleID]] = Def.setting {
    Seq(
      "org.scalactic" %%% "scalactic"  % DependencyVersions.scalatest,
      "org.scalatest" %%% "scalatest" % DependencyVersions.scalatest % Test,
      "org.typelevel" %%% "discipline-scalatest" % DependencyVersions.scalatestdiscipline  % Test
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
        // https://mvnrepository.com/artifact/org.typelevel/discipline-core
        "org.typelevel" %%% "discipline-core" % DependencyVersions.cats_discipline_core % Test
    )
  }


}
