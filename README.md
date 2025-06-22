# zpl-printer

A web tool for easily templating and printing ZPL over HTTP. Heavily inspired by
[zpl-rest](https://github.com/mrothenbuecher/zpl-rest/tree/master).

It utilizes the [Tera](https://keats.github.io/tera/docs/#templates) template
engine to construct ZPL.

![](docs/main-ui.png)

## Usage

Build with cargo. It depends on PostgreSQL or SQLite to store data, set the
`DATABASE_URL` environment variable before running. The web UI available on port
3000 allows configuring printers, label sizes, and labels.

The playground allows quick iteration of label design thanks to the label
preview service provided by [Labelary](http://labelary.com).

Labels can be saved and then printed via API.

### Host Verification (`^HV`)

When a label template contains the `^HV` command, this tool will automatically
try to parse the output to extract the values. When using it, the number of
bytes must be set exactly correct otherwise the extraction will fail.
Additionally, all fields must be set to apply to each label (the "L" flag).

This feature is not supported when printer is configured using CUPS.

### Alerts

This tool can be configured to record and notify for printer alerts.

When the `alerts_address` option is set, it listens for TCP connections on that
address from printers. Upon receiving an alert, it is saved and forwarded to any
alert targets.

## API

### `POST /api/v1/print`

Prints a label. It requires a JSON-encoded body with the following contents:

```jsonc
{
    "printer_id": "", // The printer's ID
    "label_id": "",   // The label's ID
    "data": {}        // Arbitrary key-value data passed to label's template
}
```

If the label template does not contain any `^HV` commands, no response body will
be sent and the status code will be 204.

If the template does contains an `^HV` command, and it was possible to parse the
values, they will be returned and the response will have a status code of 200:

```jsonc
{
    "verifications": [
        [ // Each printed label will have an entry
            { // And each entry is key/value pairs of the field ID and value
                "1": "value"
            }
        ]
    ]
}
```

Responses also set a `x-history-id` header with the ID of the saved history
object.
