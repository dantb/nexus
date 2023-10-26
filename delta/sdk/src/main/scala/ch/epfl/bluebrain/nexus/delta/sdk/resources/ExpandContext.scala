package ch.epfl.bluebrain.nexus.delta.sdk.resources

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import ch.epfl.bluebrain.nexus.delta.kernel.effect.migration.toCatsIOOps
import ch.epfl.bluebrain.nexus.delta.rdf.IriOrBNode.Iri
import ch.epfl.bluebrain.nexus.delta.sdk.model.{IdSegment, IdSegmentRef}
import ch.epfl.bluebrain.nexus.delta.sdk.projects.FetchContext
import ch.epfl.bluebrain.nexus.delta.sdk.projects.model.ProjectContext
import ch.epfl.bluebrain.nexus.delta.sdk.resources.ExpandContext.{Context, ExpandedContext}
import ch.epfl.bluebrain.nexus.delta.sourcing.model.Identity.Subject
import ch.epfl.bluebrain.nexus.delta.sourcing.model.{ProjectRef, ResourceRef}

trait ExpandContext {
  def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaRef: Option[IdSegment])(implicit
      c: Subject
  ): IO[ExpandedContext]
}

object ExpandContext {

  sealed trait Context
  object Context {
    final case object Read   extends Context
    final case object Create extends Context
    final case object Modify extends Context
  }

  def mk[R <: Throwable](fetchContext: FetchContext[R], mkExpansionError: String => Throwable): ExpandContext =
    new ExpandContext {

      override def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaRef: Option[IdSegment])(implicit
          c: Subject
      ): IO[ExpandedContext] =
        fetchCtx(context, pr).flatMap(pc => IO.fromEither(expandContext(mkExpansionError, id, pc, schemaRef)))

      private def fetchCtx(context: Context, pr: ProjectRef)(implicit c: Subject): IO[ProjectContext] = (context match {
        case Context.Read   => fetchContext.onRead(pr)
        case Context.Create => fetchContext.onCreate(pr)
        case Context.Modify => fetchContext.onModify(pr)
      }).toCatsIO
    }

  final case class ExpandedContext(
      iri: Iri,
      pc: ProjectContext,
      schemaRef: Option[ResourceRef]
  )

  def expandContext(
      mkExpansionError: String => Throwable,
      id: IdSegment,
      pc: ProjectContext,
      schemaSeg: Option[IdSegment]
  ): Either[Throwable, ExpandedContext] =
    for {
      ref <- expandResourceRef(mkExpansionError, schemaSeg, pc)
      iri <- expandIri(mkExpansionError, id, pc)
    } yield ExpandedContext(iri, pc, ref)

  def expandResourceRef(
      mkExpansionError: String => Throwable,
      segment: IdSegment,
      context: ProjectContext
  ): Either[Throwable, ResourceRef] =
    segment.toIri(context.apiMappings, context.base).map(ResourceRef(_)).toRight(mkExpansionError(segment.asString))

  def expandResourceRef(
      mkExpansionError: String => Throwable,
      segmentOpt: Option[IdSegment],
      context: ProjectContext
  ): Either[Throwable, Option[ResourceRef]] = segmentOpt match {
    case Some(value) => expandResourceRef(mkExpansionError, value, context).map(_.some)
    case None        => Right(None)
  }

  def expandIri(
      mkExpansionError: String => Throwable,
      segment: IdSegment,
      projectContext: ProjectContext
  ): Either[Throwable, Iri] =
    expandIri(IdSegmentRef(segment), projectContext).map(_.iri).toRight(mkExpansionError(segment.asString))

  def expandIri(segment: IdSegmentRef, projectContext: ProjectContext): Option[ResourceRef] =
    segment.value.toIri(projectContext.apiMappings, projectContext.base).map { iri =>
      segment match {
        case IdSegmentRef.Latest(_)        => ResourceRef.Latest(iri)
        case IdSegmentRef.Revision(_, rev) => ResourceRef.Revision(iri, rev)
        case IdSegmentRef.Tag(_, tag)      => ResourceRef.Tag(iri, tag)
      }
    }
}
