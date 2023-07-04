# zpl-printer

A web tool for easily templating and printing ZPL over HTTP. Heavily inspired by
[zpl-rest](https://github.com/mrothenbuecher/zpl-rest/tree/master).

It utilizes the [Tera](https://tera.netlify.app/docs/#templates) template engine
to construct ZPL.

![](docs/main-ui.png)

## Usage

Build with cargo. It depends on PostgreSQL to store data, set the `DATABASE_URL`
environment variable before running. The web UI available on port 3000 allows
configuring printers, label sizes, and labels.

The playground allows quick iteration of label design thanks to the label
preview service provided by [Labelary](http://labelary.com).

Labels can be saved and then printed via API.

### API

#### `POST /api/v1/print`

Prints a label. It requires a JSON-encoded body with the following contents:

```jsonc
{
    "printer_id": "", // The printer's ID
    "label_id": "",   // The label's ID
    "data": {}        // Arbitrary key-value data passed to label's template
}
```
