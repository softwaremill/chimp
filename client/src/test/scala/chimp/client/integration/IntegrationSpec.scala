package chimp.client.integration

import org.scalatest.{Suite, SuiteMixin}

trait IntegrationSpec extends SuiteMixin:
  this: Suite =>

  abstract override def tags: Map[String, Set[String]] =
    val base = super.tags
    testNames.foldLeft(base): (acc, name) =>
      acc.updated(name, acc.getOrElse(name, Set.empty) + "Integration")
