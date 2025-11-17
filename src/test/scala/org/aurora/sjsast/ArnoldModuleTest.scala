package org.aurora.sjsast


class ArnoldModuleTest extends BaseAsyncTest:

  "ArnoldModule-0" should {
    "be a module" in { 

      for {
        astPCM      <- parse(0)
        modulePCM   <- Future(ModulePCM(astPCM))
        assertName  <- modulePCM.module.name should be("chf_2024")
        _           <- finfo(s"${modulePCM.show}")
      } yield assertName
    } 
  }
