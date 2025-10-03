package org.aurora.sjsast

import scala.concurrent.Future


class DerivedTest extends BaseAsyncTest{

  "this" should {
    "work" in {

      info(testfile(1))
      val fut = Future(3)
      fut.map(_ + 1).map(_ should be(4))
    }
  }

  "createFileSync" should {
    "work" in {
      val path = testfile(1)
      fileutils.createFileIfNotExists(testfile(0))
      info(s"reading: ${fileutils.readFileSync(testfile(0)) }")
      // val exists = fileutils.pathExists(path)
      Future(fs.existsSync(testfile(0))).map( _ should be(true))
    }
  }
}
