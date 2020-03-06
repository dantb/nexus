package ch.epfl.bluebrain.nexus.cli

import java.time.Instant
import java.util.UUID

import cats.effect.IO
import ch.epfl.bluebrain.nexus.cli.ClientError.ServerStatusError
import ch.epfl.bluebrain.nexus.cli.EventStreamClient.{LiveEventStreamClient, TestEventStreamClient}
import ch.epfl.bluebrain.nexus.cli.types.Offset.{Sequence, TimeBasedUUID}
import ch.epfl.bluebrain.nexus.cli.types.{Event, EventEnvelope, Label, Offset}
import ch.epfl.bluebrain.nexus.cli.utils.Fixtures
import fs2.Stream
import fs2.text.utf8Encode
import org.http4s.Method._
import org.http4s.ServerSentEvent.EventId
import org.http4s.client.Client
import org.http4s.headers.{`Last-Event-Id`, Authorization}
import org.http4s.{HttpApp, Response, Status, Uri}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

class EventStreamClientSpec extends AnyWordSpecLike with Matchers with Fixtures {

  private val orgUuid      = UUID.fromString("e6a5c668-5051-49bb-9414-265ccb51323e")
  private val orgLabel     = Label("myorg")
  private val projectUuid  = UUID.fromString("0e23f7e4-0d06-4e05-b850-5944f03557af")
  private val projectLabel = Label("mylabel")

  private val id                = Uri.unsafeFromString("https://example.com/v1/myId")
  private val instantCreated    = Instant.parse("2020-03-02T11:04:48.934789Z")
  private val instantUpdated    = Instant.parse("2020-03-02T11:05:26.713124Z")
  private val instantDeprecated = Instant.parse("2020-03-02T11:05:34.643363Z")
  private val created           = Event("Created", id, orgLabel, projectLabel, Set(nxv / "TypeA", nxv / "TypeB"), instantCreated)
  private val updated           = Event("Updated", id, orgLabel, projectLabel, Set(nxv / "TypeB"), instantUpdated)
  private val deprecated        = Event("Deprecated", id, orgLabel, projectLabel, Set.empty, instantDeprecated)

  "A LiveEventStreamClient" should {

    val stream: Stream[IO, Byte] = Stream.emit(contentOf("/events.txt")).through(utf8Encode)
    val eventId: Offset          = Sequence(1L)

    val mockedHttpApp = HttpApp[IO] {
      case r
          if (r.uri == endpoints.eventsUri ||
            r.uri == endpoints.eventsUri(orgLabel) ||
            r.uri == endpoints.eventsUri(orgLabel, projectLabel)) &&
            r.method == GET &&
            r.headers.get(Authorization) == config.authorizationHeader &&
            r.headers.get(`Last-Event-Id`).contains(`Last-Event-Id`(EventId(eventId.asString))) =>
        IO.delay(Response[IO](Status.Ok).withEntity(stream))
    }

    val mockedHttpClient: Client[IO] = Client.fromHttpApp(mockedHttpApp)

    val projectClient = new ProjectClient[IO] {
      override def label(organization: UUID, project: UUID): IO[Either[ClientError, (Label, Label)]] =
        if (orgUuid == organization && project == projectUuid) IO.pure(Right(orgLabel -> projectLabel))
        else IO.pure(Left(ServerStatusError(Status.InternalServerError, "")))
    }

    val client: EventStreamClient[IO] = new LiveEventStreamClient(mockedHttpClient, projectClient, config)

    "return events" in {
      val timeCreated    = TimeBasedUUID(UUID.fromString("a2293500-5c75-11ea-beb1-a5eb66b44d1c"))
      val timeUpdated    = TimeBasedUUID(UUID.fromString("b8a93f50-5c75-11ea-beb1-a5eb66b44d1c"))
      val timeDeprecated = TimeBasedUUID(UUID.fromString("bd62f6d0-5c75-11ea-beb1-a5eb66b44d1c"))

      val expected = List(
        EventEnvelope(timeCreated, created),
        EventEnvelope(timeUpdated, updated),
        EventEnvelope(timeDeprecated, deprecated)
      )

      client(Some(eventId)).take(3).compile.to(List).unsafeRunSync() shouldEqual expected
      client(orgLabel, Some(eventId)).take(3).compile.to(List).unsafeRunSync() shouldEqual expected
      client(orgLabel, projectLabel, Some(eventId)).take(3).compile.to(List).unsafeRunSync() shouldEqual expected
    }

    "A TestEventStreamClient" should {

      val events                        = List(created, created.copy(project = Label("other")), updated, deprecated)
      val expected                      = events.zipWithIndex.map { case (ev, i) => EventEnvelope(Sequence(i + 1L), ev) }
      val client: EventStreamClient[IO] = new TestEventStreamClient(events)

      "return events" in {

        client(None).take(4).compile.to(List).unsafeRunSync() shouldEqual expected
        client(Some(Sequence(2L))).take(2).compile.to(List).unsafeRunSync() shouldEqual expected.drop(2)
        client(orgLabel, Some(Sequence(2L))).take(2).compile.to(List).unsafeRunSync() shouldEqual expected.drop(2)
        client(orgLabel, projectLabel, Some(Sequence(1L))).take(2).compile.to(List).unsafeRunSync() shouldEqual
          expected.drop(2)
      }
    }
  }
}