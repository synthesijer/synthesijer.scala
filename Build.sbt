name := "synthesijer.scala"
version := "1.0"
scalaVersion := "2.11.7"

val additionalClasses = file(sys.env("SYNTHESIJER"))
unmanagedClasspath in Compile += additionalClasses
unmanagedClasspath in Runtime += additionalClasses
