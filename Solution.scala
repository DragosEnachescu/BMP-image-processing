import util.Pixel
import util.Util.{getNeighbors, toGrayScale}

import scala.annotation.tailrec

// Online viewer: https://0xc0de.fr/webppm/

object Solution {
  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  type Sir = List[Char]

  def split(s: Sir): List[Sir] = {
    def op(c: Char, acc: List[Sir]): List[Sir] =
      acc match {
        case Nil => if (c == ' ') Nil else List(List(c))
        case x :: xs => if (c == ' ') Nil :: acc else (c :: x) :: xs
      }

    s.foldRight(Nil: List[Sir])(op)
  }

  def paste(start: Int, stop: Int, rows: List[String], length: Int): Image = {
    @tailrec
    def aux(i: Int, acc: List[Pixel], acc2: Image, count: Int): Image = {
      if (count == length) {
        aux(i, List(), acc2 ++ List(acc), 0)
      }
      else if (i > stop) acc2
      else {
        val line = split(rows(i).toList)
        val result0 = line(0).map(_.toString).mkString("").toInt
        val result1 = line(1).map(_.toString).mkString("").toInt
        val result2 = line(2).map(_.toString).mkString("").toInt

        val pixel = Pixel(result0, result1, result2)

        aux(i + 1, acc ++ List(pixel), acc2, count + 1)
      }
    }

    aux(start, List(), List(), 0)
  }

  // prerequisites
  def fromStringPPM(image: List[Char]): Image = {
    val rows = image.mkString("").split("\n").toList

    val secondLine = split(rows(1).toList)

    val number = secondLine.head.map(_.toString).mkString("").toInt

    paste(3, rows.length - 1, rows, number)
  }

  def toStringPPM(image: Image): List[Char] = {
    val row = image.head.size
    val line = image.size

    val info = s"P3\n$row $line\n255\n"

    val pixels = image.flatMap(_.flatMap(pixel => s"${pixel.red} ${pixel.green} ${pixel.blue}\n"))

    info.toList ++ pixels
  }

  // ex 1
  def verticalConcat(image1: Image, image2: Image): Image = {
    val result = image1 ++ image2

    result
  }

  def rotation90Right(startI: Int, startJ: Int, stopI: Int, stopJ: Int, image: Image): Image = {
    @tailrec
    def aux_rotation(i: Int, j: Int, acc: List[Pixel], acc2: Image): Image = {
      if (j == stopJ) {
        aux_rotation(i + 1, 0, List(), acc2 ++ List(acc))
      }
      else if (i < stopI) {
        val result0 = image(stopJ - 1 - j)(i).red
        val result1 = image(stopJ - 1 - j)(i).green
        val result2 = image(stopJ - 1 - j)(i).blue

        val pixel = Pixel(result0, result1, result2)

        aux_rotation(i, j + 1, acc ++ List(pixel), acc2)
      }
      else acc2
    }

    aux_rotation(startI, startJ, List(), List())
  }

  // ex 2
  def horizontalConcat(image1: Image, image2: Image): Image = {
    val result1 = rotation90Right(0, 0, image1.head.size, image1.size, image1)
    val result2 = rotation90Right(0, 0, image2.head.size, image2.size, image2)

    val concat = verticalConcat(result1, result2)

    val output = rotation90(0, concat.head.size - 1, concat.size - 1, 0, concat)

    output
  }

  def rotation90(startI: Int, startJ: Int, stopI: Int, stopJ: Int, image: Image): Image = {
    @tailrec
    def aux(i: Int, j: Int, acc: List[Pixel], acc2: Image): Image = {
      if (i > stopI) {
        aux(0, j - 1, List(), acc2 ++ List(acc))
      }
      else if (j >= stopJ) {
        val result0 = image(i)(j).red
        val result1 = image(i)(j).green
        val result2 = image(i)(j).blue

        val pixel = Pixel(result0, result1, result2)

        aux(i + 1, j, acc ++ List(pixel), acc2)
      }
      else acc2
    }
    aux(startI, startJ, List(), List())
  }

  def rotation180(startI: Int, startJ: Int, stopI: Int, stopJ: Int, image: Image): Image = {
    @tailrec
    def aux(i: Int, j: Int, acc: List[Pixel], acc2: Image): Image = {
      if (j >= stopJ) {
        aux(i + 1, 0, List(), acc2 ++ List(acc))
      }
      else if (i < stopI) {
        val result0 = image(image.size - i - 1)(image.head.size - 1 -  j).red
        val result1 = image(image.size - i - 1)(image.head.size - 1 -  j).green
        val result2 = image(image.size - i - 1)(image.head.size - 1 -  j).blue

        val pixel = Pixel(result0, result1, result2)

        aux(i , j + 1, acc ++ List(pixel), acc2)
      }
      else acc2
    }
    aux(startI, startJ, List(), List())
  }

  def rotation270(startI: Int, startJ: Int, stopI: Int, stopJ: Int, image: Image): Image = {
    @tailrec
    def aux(i: Int, j: Int, acc: List[Pixel], acc2: Image): Image = {
      if (j == stopJ) {
        aux(i + 1, 0, List(), acc2 ++ List(acc))
      }
      else if (i < stopI) {
        val result0 = image(image.size - j - 1)(i).red
        val result1 = image(image.size - j - 1)(i).green
        val result2 = image(image.size - j - 1)(i).blue

        val pixel = Pixel(result0, result1, result2)

        aux(i, j + 1, acc ++ List(pixel), acc2)
      }
      else acc2
    }

    aux(startI, startJ, List(), List())
  }

  // ex 3
  def rotate(image: Image, degrees: Integer): Image = {
    if (degrees == 90) {
      val imagine = rotation90(0, image.head.size - 1, image.size - 1, 0, image)
      imagine
    }
    else if (degrees == 180) {
      val imagine2 = rotation180(0, 0, image.size, image.head.size, image)
      imagine2
    }
    else if (degrees == 270) {
      println(image.head.size)
      println(image.size)
      val imagine3 = rotation270(0, 0, image.head.size, image.size, image)
      imagine3
    }
    else image
  }

  // ex 4
  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def edgeDetection(image: Image, threshold : Double): Image = {
    val grayScale = image.map(row => row.map(pixel => toGrayScale(pixel)))

    val convolutionKernel = applyConvolution(grayScale, gaussianBlurKernel)

    val Mx = applyConvolution(convolutionKernel, Gx)
    val My = applyConvolution(convolutionKernel, Gy)

    val sum = Mx.zip(My).map {
      case (rowMx, rowMy) => {
        rowMx.zip(rowMy).map {
          case (x, y) => math.abs(x) + math.abs(y)
        }
      }
    }.map {
      line =>
        line.map {
          value => {
            if (value < threshold) Pixel(0, 0, 0)
            else Pixel(255, 255, 255)
          }
        }
    }

    sum
  }

  def convolution(stopI: Int, stopJ: Int, image: List[List[GrayscaleImage]], kernel: GrayscaleImage): GrayscaleImage = {
    @tailrec
    def conv_aux(i: Int, j: Int, acc: GrayscaleImage, acc2: List[Double]): GrayscaleImage = {
      if (i >= stopI) acc
      else if (j < stopJ) {
        val ok = image(i)(j).zip(kernel).flatMap { case (row1, row2) => row1.zip(row2).map { case (x, y) => x * y } }

        conv_aux(i, j + 1, acc, acc2.appended(ok.sum))
      }
      else {
        conv_aux(i + 1, 0, acc.appended(acc2), List())
      }
    }

    conv_aux(0, 0, List(), List())
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage): GrayscaleImage = {
    val neighbour = getNeighbors(image, kernel.size / 2)

    val output = convolution(neighbour.size, neighbour(0).size, neighbour, kernel)

    output
  }

  def pascal(stopI: Integer, stopJ: Integer, m: Integer): List[List[Integer]] = {
    @tailrec
    def aux(i: Integer, j: Integer, acc: List[List[Integer]], acc2: List[Integer]): List[List[Integer]] = {
      if (i >= stopI) acc
      else if (j >= stopJ) {
        aux(i + 1, 0, acc.appended(acc2), List())
      }
      else {
        if (i == j || j == 0) {
          val x = 1
          aux(i, j + 1, acc, acc2.appended(x))
        }
        else if (i < j) {
          val x = 4 //orice valoare care nu este 0, 1, 2 sau 3
          aux(i, j + 1, acc, acc2.appended(x))
        }
        else {
          val x = ((acc(i - 1)(j) % m) + (acc(i - 1)(j - 1) % m)) % m
          aux(i, j + 1, acc, acc2.appended(x))
        }
      }
    }

    aux(0, 0, List(), List())
  }

  // ex 5
  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    val matrix = pascal(size, size, m)

    val image = matrix.map {
      row =>
        row.map(x => funct(x))
    }

    image
  }
}
