// import zio._

// case class UserConfig(needsId: Boolean, defaultId: Int)

// // 模擬獲取使用者設定
// def getUserConfig: ZIO[Any, String, UserConfig] =
//   ZIO.succeed(UserConfig(needsId = true, defaultId = 100))

// // 模擬根據設定獲取使用者 ID
// def getUserID_V2(config: UserConfig): ZIO[Any, String, Int] =
//   if (config.needsId) {
//     ZIO.succeed(123) // 假設主要 ID
//   } else {
//     ZIO.succeed(config.defaultId)
//   }

// // 複用之前的 getUserName
// def getUserName(id: Int): ZIO[Any, String, String] = {
//   if (id == 123) ZIO.succeed("Alice (from V2 pipeline)")
//   else if (id == 100) ZIO.succeed("Bob (default user)")
//   else ZIO.fail(s"User with id $id not found (V2)")
// }

// object ForComprehensionExample extends ZIOAppDefault {

//   // 使用 for-comprehension 來定義這個多步驟的流程
//   val complexPipeline: ZIO[Any, String, String] = for {
//     config    <- getUserConfig             // 步驟 1: 獲取設定
//     userId    <- getUserID_V2(config)      // 步驟 2: 根據設定獲取 ID
//     userName  <- getUserName(userId)       // 步驟 3: 根據 ID 獲取名稱
//   } yield userName // for 推導式的最終結果是 userName

//   // 測試另一種情況，假設設定不需要 ID
//   val alternativeConfigEffect: ZIO[Any, String, UserConfig] =
//     ZIO.succeed(UserConfig(needsId = false, defaultId = 100))

//   val alternativePipeline: ZIO[Any, String, String] = for {
//     config    <- alternativeConfigEffect
//     userId    <- getUserID_V2(config)
//     userName  <- getUserName(userId)
//   } yield userName


//   def run = for {
//     name1 <- complexPipeline
//     _     <- Console.printLine(s"Complex pipeline result: $name1")
//     name2 <- alternativePipeline
//     _     <- Console.printLine(s"Alternative pipeline result: $name2")
//   } yield ()
// }

import zio._

object CatchAllExample extends ZIOAppDefault {

  // 修正版本
  def parseInt(s: String): ZIO[Any, String, Int] =
    ZIO.attempt(s.toInt)
      .mapError {
        case e: NumberFormatException => s"'$s' is not a valid integer. (Caught by ZIO.attempt)"
        case t: Throwable             => s"An unexpected error occurred while parsing '$s': ${t.getMessage}"
      }

  val validString = "123"
  val invalidString = "abc"
  // val anotherInvalidString = "xyz" // 這行目前沒用到，可以註解掉或保留

  def parseWithFallback(s: String): ZIO[Any, Nothing, Int] =
    parseInt(s).catchAll { error =>
      Console.printLine(s"Parsing failed for '$s' with error: $error. Using fallback.").orDie *>
      ZIO.succeed(0)
    }

  val effect1: ZIO[Any, String, String] = ZIO.fail("Primary source failed")
  val effect2: ZIO[Any, String, String] = ZIO.fail("Secondary source also failed")
  val effect3: ZIO[Any, Nothing, String] = ZIO.succeed("Tertiary source (guaranteed success)")

  // tryMultipleSources 的型別是 ZIO[Any, Nothing, String]
  val tryMultipleSources: ZIO[Any, Nothing, String] =
    effect1.catchAll { _ =>
      Console.printLine("Effect 1 failed, trying effect 2").orDie *>
      effect2.catchAll { _ =>
        Console.printLine("Effect 2 failed, trying effect 3").orDie *>
        effect3
      }
    }

  // 恢復完整的 run 方法，但修正了 tryMultipleSources 的使用
  def run = for {
    num1 <- parseWithFallback(validString)
    _    <- Console.printLine(s"Parsed '$validString': $num1")
    num2 <- parseWithFallback(invalidString)
    _    <- Console.printLine(s"Parsed '$invalidString': $num2")
    _    <- Console.printLine("--- Trying multiple sources ---")
    finalResult <- tryMultipleSources // <--- 重要修正：直接使用，不加 .catchAll
    _    <- Console.printLine(s"Result from multiple sources: $finalResult")
  } yield ()
}