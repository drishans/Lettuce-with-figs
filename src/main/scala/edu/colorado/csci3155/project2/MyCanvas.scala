package edu.colorado.csci3155.project2

/* A class to maintain a canvas object.
*  A canvas is a collection of circles and polygons.
*/

import java.awt.geom.{Ellipse2D, Rectangle2D}
import java.awt.{Graphics2D}
import Ordering.Double

/* A figure is a sealed trait. It can be a Polygon or a "MyCircle"*/
sealed trait Figure {
    def getBoundingBox: (Double, Double, Double, Double)
    def translate(shiftX: Double, shiftY: Double): Figure
    def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double): Unit
}

/*
 Class Polygon
   A polygon is defined by a list of its vertices
 */

case class Polygon(val cList: List[(Double, Double)]) extends Figure {
    // Define the bounding box of the polygon
    override def getBoundingBox: (Double, Double, Double, Double ) = {
        val xMin = cList.map (elt => elt._1) . min
        val xMax = cList.map (elt => elt._1) . max
        val yMin = cList.map (elt => elt._2) . min
        val yMax = cList.map (elt => elt._2) . max
        (xMin, xMax, yMin, yMax)
    }
    // Create a new polygon by shifting each vertex in cList by (x,y)
    override def translate(shiftX: Double, shiftY: Double): Polygon = {
        Polygon(cList.map(elt => (elt._1 + shiftX, elt._2 + shiftY)))
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val xPoints: Array[Int] = new Array[Int](cList.length)
        val yPoints: Array[Int] = new Array[Int](cList.length)
        for (i <- 0 until cList.length){
            xPoints(i) = ((cList(i)._1 + shiftX )* scaleX).toInt
            yPoints(i) = ((cList(i)._2 + shiftY) * scaleY).toInt
        }
        g.drawPolygon(xPoints, yPoints, cList.length)
    }
}

/*
  Class MyCircle
  Define a circle with a given center c and radius r
 */
case class MyCircle(val c: (Double, Double), val r: Double) extends Figure {
    // Define the bounding box for the circle
    override def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = c._1 - r
        val xMax = c._1 + r
        val yMin = c._2 - r
        val yMax = c._2 + r
        (xMin, xMax, yMin, yMax)
    }


    // Create a new circle by shifting the center
    override def translate(shiftX: Double, shiftY: Double): MyCircle = {
        MyCircle((c._1 + shiftX, c._2 + shiftY), r)
    }

    // Function: render -- draw the polygon. Do not edit this function.
    override def render(g: Graphics2D, scaleX: Double, scaleY: Double, shiftX: Double, shiftY: Double) = {
        val centerX = ((c._1 + shiftX) * scaleX) .toInt
        val centerY = ((c._2 + shiftY) * scaleY) .toInt
        val radX = (r * scaleX).toInt
        val radY = (r * math.abs(scaleY)).toInt
        //g.draw(new Ellipse2D.Double(centerX, centerY, radX, radY))
        g.drawOval(centerX-radX, centerY-radY, 2*radX, 2*radY)
    }
}

/*
  Class : MyCanvas
  Define a canvas through a list of figure objects. Figure objects can be circles or polygons.
 */
class MyCanvas (val listOfObjects: List[Figure]) {
    // Write a function to get the bounding box for the entire canvas.
    def getBoundingBox: (Double, Double, Double, Double) = {
        val xMin = listOfObjects.map (elt => elt.getBoundingBox._1) . min
        val xMax = listOfObjects.map (elt => elt.getBoundingBox._2) . max
        val yMin = listOfObjects.map (elt => elt.getBoundingBox._3) . min
        val yMax = listOfObjects.map (elt => elt.getBoundingBox._4) . max
        (xMin, xMax, yMin, yMax)
    }

    // Write a function to translate each figure in the canvas by shiftX, shiftY
    def translate(shiftX: Double, shiftY: Double): MyCanvas = {
        new MyCanvas(listOfObjects.map(obj => obj.translate(shiftX, shiftY)))
    }

    // Write a function that will return a new MyCanvas object that places
    // all the objects in myc2 to the right of the objects in this MyCanvas.
    def placeRight(myc2: MyCanvas):MyCanvas = {
        val (xMin1, xMax1, yMin1, yMax1) = getBoundingBox
        val (xMin2, xMax2, yMin2, yMax2) = myc2.getBoundingBox
        val shiftX = xMax1 - xMin1
        val shiftY = (yMax1 - yMin1)/2 - (yMax2 - yMin2)/2
        val c2 = myc2.translate(shiftX, shiftY)
        overlap(c2)
    }

    // Write a function that will return a new MyCanvas object that places
    // all the figures in myc2 on top of the figures in this MyCanvas.
    def placeTop(myc2: MyCanvas): MyCanvas = {
        val (xMin1, xMax1, yMin1, yMax1) = getBoundingBox
        val (xMin2, xMax2, yMin2, yMax2) = myc2.getBoundingBox
        val shiftX = (xMax1 - xMin1)/2 - (xMax2 - xMin2)/2
        val shiftY = yMax1 - yMin1
        val c2 = myc2.translate(shiftX, shiftY)
        overlap(c2)
    }

    // Write a function that will rotate each figure in the canvas using
    // the angle `ang` defined in radians.
    def rotate(angRad: Double): MyCanvas = {
        def rotCalc(x:Double, y:Double): (Double, Double) = {
            (x*Math.cos(angRad) - y*Math.sin(angRad), x*Math.sin(angRad) + y*Math.cos(angRad))
        }
        new MyCanvas(listOfObjects.map {
            case Polygon(cList) => Polygon(cList.map(elt => rotCalc(elt._1, elt._2)))
            case MyCircle(c, r) => MyCircle(rotCalc(c._1, c._2), r)
        })
    }

    //Function: overlap
    // This function takes a list of objects from this canvas
    // and concatenates with a list of objects from canvas c2.
    // The result is a new MyCanvas object with the concatenated
    // list of objects.
    def overlap(c2: MyCanvas): MyCanvas = {
        new MyCanvas(listOfObjects ++ c2.listOfObjects)
    }


    // Function to draw the canvas. Do not edit.
    def render(g: Graphics2D, xMax: Double, yMax: Double) = {
        val (lx1, ux1, ly1, uy1) = this.getBoundingBox
        val shiftx = -lx1
        val shifty = -uy1
        val scaleX = xMax/(ux1 - lx1  + 1.0)
        val scaleY = yMax/(uy1 - ly1 + 1.0)
        listOfObjects.foreach(f => f.render(g,scaleX, -scaleY, shiftx, shifty))
    }

    // DO NOT EDIT THE CODE BELOW
    override def toString: String = {
        listOfObjects.foldLeft[String] ("") { case (acc, fig) => acc ++ fig.toString }
    }
    // DO NOT EDIT
    def getListOfObjects: List[Figure] = listOfObjects

    // DO NOT EDIT
    def numPolygons: Int =
        listOfObjects.count {
            case Polygon(_) => true
            case _ => false }

    //DO NOT EDIT
    def numCircles: Int = {
        listOfObjects.count {
            case MyCircle(_,_) => true
            case _ => false }
    }
    //DO NOT EDIT
    def numVerticesTotal: Int = {
        listOfObjects.foldLeft[Int](0) ((acc, f) =>
            f match {
                case Polygon(lst1) => acc + lst1.length
                case _ => acc
            }
        )
    }
}
