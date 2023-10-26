package ch.epfl.bluebrain.nexus.delta.sdk.resources

import cats.effect.IO
import cats.implicits.catsSyntaxOptionId
import ch.epfl.bluebrain.nexus.delta.kernel.effect.migration.toCatsIOOps
import ch.epfl.bluebrain.nexus.delta.rdf.IriOrBNode.Iri
import ch.epfl.bluebrain.nexus.delta.sdk.model.{IdSegment, IdSegmentRef}
import ch.epfl.bluebrain.nexus.delta.sdk.projects.FetchContext
import ch.epfl.bluebrain.nexus.delta.sdk.projects.model.ProjectContext
import ch.epfl.bluebrain.nexus.delta.sourcing.model.Identity.Subject
import ch.epfl.bluebrain.nexus.delta.sourcing.model.{ProjectRef, ResourceRef}

trait ExpandContext {
  def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaId: Option[IdSegment])(implicit
      c: Subject
  ): IO[ResourceContext]

  def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaId: IdSegment)(implicit
      c: Subject
  ): IO[ResourceContextWithSchema]
}

final case class ResourceContext(
    iri: Iri,
    pc: ProjectContext,
    schemaRef: Option[ResourceRef]
)

final case class ResourceContextWithSchema(
    iri: Iri,
    pc: ProjectContext,
    schemaRef: ResourceRef
)

sealed trait Context

object Context {
  final case object Read extends Context

  final case object Create extends Context

  final case object Modify extends Context
}

object ExpandContext {

  def mk[R <: Throwable](fetchContext: FetchContext[R], mkExpansionError: String => Throwable): ExpandContext =
    new ExpandContext {

      override def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaId: Option[IdSegment])(implicit
          c: Subject
      ): IO[ResourceContext] =
        fetchCtx(context, pr).flatMap(pc => IO.fromEither(expand(id, pc, schemaId)))

      override def apply(context: Context, id: IdSegment, pr: ProjectRef, schemaId: IdSegment)(implicit
          c: Subject
      ): IO[ResourceContextWithSchema] =
        fetchCtx(context, pr).flatMap(pc => IO.fromEither(expandWithSchema(id, pc, schemaId)))

      private def fetchCtx(context: Context, pr: ProjectRef)(implicit c: Subject): IO[ProjectContext] = (context match {
        case Context.Read => fetchContext.onRead(pr)
        case Context.Create => fetchContext.onCreate(pr)
        case Context.Modify => fetchContext.onModify(pr)
      }).toCatsIO

      private def expand(id: IdSegment, pc: ProjectContext, schemaSeg: Option[IdSegment]) = for {
        ref <- expandSchemaId(mkExpansionError, schemaSeg, pc)
        iri <- expandIri(mkExpansionError, id, pc)
      } yield ResourceContext(iri, pc, ref)

      private def expandWithSchema(id: IdSegment, pc: ProjectContext, schemaSeg: IdSegment) = for {
        ref <- expandSchemaId(mkExpansionError, schemaSeg, pc)
        iri <- expandIri(mkExpansionError, id, pc)
      } yield ResourceContextWithSchema(iri, pc, ref)
    }

  def expandSchemaId(
      mkExpansionError: String => Throwable,
      segment: IdSegment,
      context: ProjectContext
  ): Either[Throwable, ResourceRef] =
    segment.toIri(context.apiMappings, context.base).map(ResourceRef(_)).toRight(mkExpansionError(segment.asString))

  def expandSchemaId(
      mkExpansionError: String => Throwable,
      segmentOpt: Option[IdSegment],
      context: ProjectContext
  ): Either[Throwable, Option[ResourceRef]] = segmentOpt match {
    case Some(value) => expandSchemaId(mkExpansionError, value, context).map(_.some)
    case None        => Right(None)
  }

  def expandIri(
      mkExpansionError: String => Throwable,
      segment: IdSegment,
      projectContext: ProjectContext
  ): Either[Throwable, Iri] =
    ProjectContext.segmentToResourceRef(projectContext, IdSegmentRef(segment)).map(_.iri).toRight(mkExpansionError(segment.asString))

}
