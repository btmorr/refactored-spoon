# refactored-spoon
An exercise begun at NEScala 2017 to use the Free Monad and Free Applicative to model a practical graph of computations

The idea is to take a real sequence of modular computations, and stitch them together in a way that defines a graph of computations that involves some sequential dependencies, some parallelizable tasks, and somewhat arbitrary fan-out and fan-in between nodes. This is a beginner's first foray into Free.
