import org.scalatest._

class ImportDataSpec extends FunSpec with Matchers {
  describe("#rateChar") {
    it("should match") {
      ImportData.rateChar('ビ') should be(ImportData.NoobKyu)
    }
  }

  describe("#rate") {
    it("should work for several chars") {
      ImportData.rateWord("ビリビリ") should be(Some(ImportData.NoobKyu))
    }
  }
}
