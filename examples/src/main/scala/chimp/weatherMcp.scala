package chimp

//> using dep com.softwaremill.chimp::core:0.1.2
//> using dep com.softwaremill.sttp.client4::core:4.0.8

import chimp.*
import io.circe.Codec
import io.circe.parser.decode
import ox.either
import ox.either.ok
import sttp.client4.*
import sttp.tapir.*
import sttp.tapir.server.netty.sync.NettySyncServer

case class WeatherInput(city: String) derives Codec, Schema

// Represents a single result from Nominatim geocoding API
case class NominatimResult(lat: String, lon: String, display_name: String) derives Codec

// Represents the response from Open-Meteo API (current weather)
case class OpenMeteoCurrentWeather(temperature: Double, weathercode: Int) derives Codec
case class OpenMeteoResponse(current_weather: OpenMeteoCurrentWeather) derives Codec

@main def weatherMcp(): Unit =
  val sttpBackend = DefaultSyncBackend()

  val weatherTool = tool("weather")
    .description("Checks the weather in the given city")
    .input[WeatherInput]
    .handle: input =>
      either:
        val geocodeResult = geocodeCity(input.city, sttpBackend).ok()
        val weatherResult = fetchWeather(geocodeResult.lat, geocodeResult.lon, sttpBackend).ok()
        weatherDescription(geocodeResult.display_name, weatherResult.temperature, weatherResult.weathercode)

  val mcpServerEndpoint = mcpEndpoint(List(weatherTool), List("mcp"))
  NettySyncServer().port(8080).addEndpoint(mcpServerEndpoint).startAndWait()

/** Maps Open-Meteo weather codes to human-readable descriptions. */
private val weatherCodeDescriptions = Map(
  0 -> "Clear sky",
  1 -> "Mainly clear",
  2 -> "Partly cloudy",
  3 -> "Overcast",
  45 -> "Fog",
  48 -> "Depositing rime fog",
  51 -> "Drizzle: Light",
  53 -> "Drizzle: Moderate",
  55 -> "Drizzle: Dense",
  56 -> "Freezing Drizzle: Light",
  57 -> "Freezing Drizzle: Dense",
  61 -> "Rain: Slight",
  63 -> "Rain: Moderate",
  65 -> "Rain: Heavy",
  66 -> "Freezing Rain: Light",
  67 -> "Freezing Rain: Heavy",
  71 -> "Snow fall: Slight",
  73 -> "Snow fall: Moderate",
  75 -> "Snow fall: Heavy",
  77 -> "Snow grains",
  80 -> "Rain showers: Slight",
  81 -> "Rain showers: Moderate",
  82 -> "Rain showers: Violent",
  85 -> "Snow showers: Slight",
  86 -> "Snow showers: Heavy",
  95 -> "Thunderstorm: Slight or moderate",
  96 -> "Thunderstorm with slight hail",
  99 -> "Thunderstorm with heavy hail"
)

/** Geocodes a city name to (latitude, longitude) using the Nominatim API. */
private def geocodeCity(city: String, backend: SyncBackend): Either[String, NominatimResult] =
  val nominatimUrl = uri"https://nominatim.openstreetmap.org/search?format=json&limit=1&q=$city"
  val geoResp = basicRequest.get(nominatimUrl).header("User-Agent", "chimp-weather-tool").send(backend)
  geoResp.body match
    case Left(_) => Left(s"Failed to contact geocoding service: ${geoResp.code}")
    case Right(body) =>
      decode[List[NominatimResult]](body) match
        case Left(err)               => Left(s"Failed to decode geocoding response: $err")
        case Right(Nil)              => Left(s"City not found: $city")
        case Right(firstResult :: _) => Right(firstResult)

/** Fetches the current weather for the given coordinates using the Open-Meteo API. */
private def fetchWeather(lat: String, lon: String, backend: SyncBackend): Either[String, OpenMeteoCurrentWeather] =
  val meteoUrl = uri"https://api.open-meteo.com/v1/forecast?latitude=$lat&longitude=$lon&current_weather=true"
  val meteoResp = basicRequest.get(meteoUrl).send(backend)
  meteoResp.body match
    case Left(_) => Left(s"Failed to contact weather service: ${meteoResp.code}")
    case Right(body) =>
      decode[OpenMeteoResponse](body) match
        case Left(err)    => Left(s"Failed to decode weather response: $err")
        case Right(meteo) => Right(meteo.current_weather)

/** Maps an Open-Meteo weather code to a human-readable description. */
private def weatherDescription(where: String, temp: Double, code: Int): String =
  s"The weather in $where is ${weatherCodeDescriptions.getOrElse(code, s"Unknown (code $code)").toLowerCase()}, with a high of $temp celsius."
