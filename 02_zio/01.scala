// import zio._

// object HelloZIO extends ZIOAppDefault { // ZIOAppDefault 提供了一個執行 ZIO 效果的環境

//   // 1. 建立一個 ZIO 效果，它會成功並回傳一個字串 "Hello, ZIO!"
//   val successfulEffect: ZIO[Any, Nothing, String] = ZIO.succeed("Hello, ZIO!")

//   // 為了執行並看到結果，我們在 run 方法中使用它
//   // `map` 可以讓我們轉換成功的值，`ZIO.debug` 會印出值並回傳它
//   def run = successfulEffect.flatMap(message => Console.printLine(message))
// }

// import zio._

// object FailingZIO extends ZIOAppDefault {

//   // 2. 建立一個 ZIO 效果，它會失敗並回傳一個錯誤 (這裡用 String 表示錯誤)
//   val failedEffect: ZIO[Any, String, Nothing] = ZIO.fail("Oh no, something went wrong!")

//   // 執行這個效果
//   // 當 ZIO 效果失敗時，ZIOAppDefault 預設會印出錯誤訊息
//   // 為了更明確地看到我們如何處理它，我們可以加上 .either
//   // .either 會將 ZIO[R, E, A] 轉換為 ZIO[R, Nothing, Either[E, A]]
//   // 這樣失敗就變成了 Either 中的 Left 值，成功則是 Right 值
//   def run = failedEffect.either.flatMap {
//     case Left(error)   => Console.printLine(s"Caught error: $error")
//     case Right(value)  => Console.printLine(s"Got value: $value") // 這行不會執行
//   }
// }