# coursera_scala

forcomp心得

1. 
/** Converts a sentence into its character occurrence list. */
def sentenceOccurrences(s: Sentence): Occurrences
容易把上面这个函数实现成为s flatMap (wordOccurrences(_))，还没发现错误了。因为工程里自带的单元测试发现不了。
  test("sentenceOccurrences: abcd e") {
    assert(sentenceOccurrences(List("abcd", "e")) === List(('a', 1), ('b', 1), ('c', 1), ('d', 1), ('e', 1)))
  }
直到做了最后一题 def sentenceAnagrams(sentence: Sentence): List[Sentence]。才发现基础设施有问题。于是增加单元测试
  test("sentenceOccurrences: ") {
    val sentence = List("Linux", "rulez")
    assert(sentenceOccurrences(sentence) === List(('e', 1), ('i', 1), ('l', 2), ('n', 1), ('r', 1), ('u', 2), ('x', 1), ('z', 1)))
  }

那么问题就会暴露了：
List((i,1), (l,1), (n,1), (u,1), (x,1), (e,1), (l,1), (r,1), (u,1), (z,1)) 
did not equal 
List((e,1), (i,1), (l,2), (n,1), (r,1), (u,2), (x,1), (z,1))
分别统计每个词的词频把结果装一起，忽略了合并相同字母的词频。

不如一开始就把所有的词语搞成一个：
def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences(s.mkString(""))

mkString还是应该经常念起的
  def mkString(sep: String): String = mkString("", sep, "")
  def mkString: String = mkString("")

2.
  /** Returns the list of all subsets of the occurrence list.
    * This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
    * is a subset of `List(('k', 1), ('o', 1))`.
    * It also include the empty subset `List()`.
    *
    * Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
    *
    * List(
    * List(),
    * List(('a', 1)),
    * List(('a', 2)),
    * List(('b', 1)),
    * List(('a', 1), ('b', 1)),
    * List(('a', 2), ('b', 1)),
    * List(('b', 2)),
    * List(('a', 1), ('b', 2)),
    * List(('a', 2), ('b', 2))
    * )
    *
    * Note that the order of the occurrence list subsets does not matter -- the subsets
    * in the example above could have been displayed in some other order.
    */
    
这个题目读了之后会立刻形成一个思路，就是“少一个字符出现一次的combinations”和"有这个字符出现一次的combinations"的笛卡尔积, 于是写出如下方法 
 def combinations(occurrences: Occurrences): List[Occurrences] = {
    def comb(occList: List[Occurrences], p: (Char, Int)): List[Occurrences] =
      for {
        o <- occList
        t <- 0 to p._2
        n = (p._1, t)
      } yield n :: o filter (_._2 > 0)

    occurrences match {
      case Nil => List(List())
      case (c, cnt) :: tail => {
        if (cnt > 2)
          comb(combinations((c, cnt - 1) :: tail), (c, cnt))
        else
          comb(combinations(tail), (c, cnt))
      }
    }

  }
  
其实还可以认为“不出现这个字符的combinations”和“有这个字符出现若干次的combinations”的笛卡尔积。于是有下面方法：
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case head :: tail => {
      for (c <- 0 to head._2; d <- combinations(tail)) yield {
        if (c > 0) List((head._1, c)) ++ d
        else d
      }
    }.toList
  }  
  
总之，这个题目用上for yield感觉是较为方便的。  

3. 
  /** Returns a list of all anagram sentences of the given sentence.
    *
    * An anagram of a sentence is formed by taking the occurrences of all the characters of
    * all the words in the sentence, and producing all possible combinations of words with those characters,
    * such that the words have to be from the dictionary.
    *
    * The number of words in the sentence and its anagrams does not have to correspond.
    * For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
    *
    * Also, two sentences with the same words but in a different order are considered two different anagrams.
    * For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
    * `List("I", "love", "you")`.
    *
    * Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
    *
    * List(
    * List(en, as, my),
    * List(en, my, as),
    * List(man, yes),
    * List(men, say),
    * List(as, en, my),
    * List(as, my, en),
    * List(sane, my),
    * List(Sean, my),
    * List(my, en, as),
    * List(my, as, en),
    * List(my, sane),
    * List(my, Sean),
    * List(say, men),
    * List(yes, man)
    * )
    *
    * The different sentences do not have to be output in the order shown above - any order is fine as long as
    * all the anagrams are there. Every returned word has to exist in the dictionary.
    *
    * Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
    * so it has to be returned in this list.
    *
    * Note: There is only one anagram of an empty sentence.
    */

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagrams(occurrences: Occurrences): List[Sentence] = occurrences match {
      case Nil => List(List())
      case head :: tail => {
        val cob = combinations(occurrences)
        for {
          o <- cob if(dictionaryByOccurrences.keySet(o))
          w <- dictionaryByOccurrences(o)
          k <- sentenceAnagrams(subtract(occurrences, o))
        } yield w :: k
      }
    }
    sentenceAnagrams(sentenceOccurrences(sentence))
  }
  
 此题目我想说，大胆地使用可爱的for generator吧。这个语法糖真的不负Martin的宣传语 - “they can make combinatorial problems easier to solve”
 （摘自<Programming in Scala> Chapter23 - For Expressions Revisited）
 
 思路是，找到一个字典里有的词频，拿到所有的可能性，递归其余词频（借助subtract函数）并和前者笛卡尔积。
 
  
  


  
  

