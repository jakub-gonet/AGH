defmodule Pollution.Data.Stream do
  defp parse_line(line) when is_binary(line) do
    line
    |> String.trim()
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
      str: Enum.join([date, time, long, lat, level], ","),
      datetime:
        {date |> to_iso_date.() |> Date.from_iso8601!() |> Date.to_erl(),
         time |> to_iso_time.() |> Time.from_iso8601!() |> Time.to_erl()},
      location: {String.to_float(long), String.to_float(lat)},
      pollution_level: String.to_integer(level)
    }
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
    filename
    |> File.stream!()
    |> Stream.map(&parse_line/1)
    |> Stream.map(fn data ->
      station_retval =
        case insert_station(data) do
          :ok -> :ok
          {:error, :duplicate_station} -> :ok
          {:error, _} = e -> e
        end

      measurement_retval = insert_measurement(data)

      %{station: station_retval, measurement: measurement_retval}
    end)
    |> Enum.reduce([], fn
      %{station: :ok, measurement: :ok}, ret_acc -> ret_acc
      err, ret_acc -> [err | ret_acc]
    end)
    |> case do
      [] -> :ok
      err -> {:error, err}
    end
  end
end
