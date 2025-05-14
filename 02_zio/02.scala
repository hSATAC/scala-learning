// import zio._

// object MapExample extends ZIOAppDefault {

//   val numberEffect: ZIO[Any, Nothing, Int] = ZIO.succeed(10)

//   // 使用 map 將成功的數字乘以 2
//   val doubledEffect: ZIO[Any, Nothing, Int] = numberEffect.map(n => n * 2)
//   // 更簡潔的寫法: val doubledEffect = numberEffect.map(_ * 2)

//   def run = doubledEffect.flatMap(result => Console.printLine(s"The doubled number is: $result"))
// }

import zio._

object FlatMapExample extends ZIOAppDefault {

  // 效果 1: 獲取使用者 ID (假設它總是成功)
  val getUserID: ZIO[Any, Nothing, Int] = ZIO.succeed(123)

  // 一個函式，根據 ID 建立一個 "獲取使用者名稱" 的效果
  // 假設這個操作也可能失敗 (例如，找不到使用者)
  def getUserName(id: Int): ZIO[Any, String, String] = {
    if (id == 123) {
      ZIO.succeed("Alice")
    } else {
      ZIO.fail(s"User with id $id not found")
    }
  }

  // 使用 flatMap 來串聯這兩個操作
  val pipeline: ZIO[Any, String, String] = getUserID.flatMap { id =>
    // id 是 getUserID 成功的結果
    // 我們現在回傳 getUserName(id) 這個 *新的 ZIO 效果*
    getUserName(id)
  }

  // 另一個測試，如果 ID 不對會發生什麼
  val failingPipeline: ZIO[Any, String, String] = ZIO.succeed(999).flatMap { id =>
    getUserName(id)
  }

  def run = for {
    name <- pipeline
    _    <- Console.printLine(s"Successfully fetched user: $name")
    _    <- Console.printLine("--- Now trying the failing pipeline ---")
    // 我們用 .either 來安全地處理可能的失敗，避免程式直接終止
    result <- failingPipeline.either
    _    <- result match {
              case Left(error)   => Console.printLine(s"Failing pipeline reported: $error")
              case Right(value)  => Console.printLine(s"Failing pipeline reported: $value (should not happen)")
            }
  } yield ()
}