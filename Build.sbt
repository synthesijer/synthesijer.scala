name := "synthesijer.scala"
version := "1.0"
scalaVersion := "2.11.7"

unmanagedSourceDirectories in Compile += file(baseDirectory.value + "/src/samples")
unmanagedSourceDirectories in Runtime += file(baseDirectory.value + "/src/samples")

val additionalClasses = file(sys.env("SYNTHESIJER"))
unmanagedClasspath in Compile += additionalClasses
unmanagedClasspath in Runtime += additionalClasses

val extraLibClasses = file(sys.env("SYNTHESIJER_EXTRA_LIB")+"/bin")
unmanagedClasspath in Compile += extraLibClasses
unmanagedClasspath in Runtime += extraLibClasses

