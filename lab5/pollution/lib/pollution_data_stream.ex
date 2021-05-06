defmodule Pollution.Data.Stream do
  defp import_CSV_data(filename) do
    filename
    |> File.read!()
    |> String.split("\r\n")
  end

  defp parse_line(line) when is_binary(line) do
    line
    |> String.split(",")
    |> parse_line()
  end

  defp parse_line([date, time, long, lat, level]) do
    to_iso_date = fn str ->
      str
      |> String.split("-")
      |> Enum.reverse()
      |> Enum.join("-")
    end

    to_iso_time = fn
      <<hour::binary-size(2)>> <> ":60" ->
        next_hour =
          (String.to_integer(hour) + 1)
          |> rem(24)
          |> Integer.to_string()
          |> String.pad_leading(2, "0")

        "#{next_hour}:00:00"

      str ->
        "#{str}:00"
    end

    %{
      datetime:
        {date |> to_iso_date.() |> Date.from_iso8601!() |> Date.to_erl(),
         time |> to_iso_time.() |> Time.from_iso8601!() |> Time.to_erl()},
      location: {String.to_float(long), String.to_float(lat)},
      pollution_level: String.to_integer(level)
    }
  end

  defp identify_stations(parsed) do
    Enum.uniq_by(parsed, fn %{location: loc} -> loc end)
  end

  defp get_station_id({long, lat}) do
    "station_#{long}_#{lat}"
  end

  defp insert_station(%{location: loc}) do
    :pollution_server.add_station(get_station_id(loc), loc)
  end

  defp insert_measurement(%{datetime: date, location: loc, pollution_level: level}) do
    :pollution_server.add_value(get_station_id(loc), date, "PM10", level)
  end

  def load_data(filename) do
    parsed =
      filename
      |> import_CSV_data()
      |> Enum.map(&parse_line/1)

    f = fn ->
      parsed
      |> identify_stations()
      |> Enum.map(&insert_station/1)
    end

    f2 = fn -> Enum.map(parsed, &insert_measurement/1) end
    {:timer.tc(f) |> elem(0), :timer.tc(f2) |> elem(0)}
  end
end
