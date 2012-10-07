package cc.factorie.debug

import cc.factorie._

/**
 * Test whether custom Diff implementations are correct
 * @author Michael Wick and Sameer Singh
 * @since 0.9
 * @see DiffList
 */

object DiffChecker {
  // Scores a given difflist according to the model, and compares with exhaustively scoring previous
  // and next configurations (of the given variables)
  def diffListScore(diffList: DiffList, variables: Iterable[Variable], model: TemplateModel): Boolean = {
    // TODO check variables in difflist are in variables
    diffList.redo
    val diffScore:Double = diffList.scoreAndUndo(model)
    val prevScore:Double = model.sumScore(variables)
    diffList.redo
    val modelScoreDiff:Double = model.sumScore(variables)
    diffScore == modelScoreDiff
  }

}