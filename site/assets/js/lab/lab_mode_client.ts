import { JSParserConfig } from "js/radlr/radlr_wasm";
import { Eventable } from "./eventable";


enum WSRequestCodes {
  BUILD_GRAMMAR = 1,
}


type RadlrWSClientEvents = {
  "closed": void
  "grammar-built": void
}

class RadlrWSClient extends Eventable<RadlrWSClientEvents> {
  port = "15421";
  private host = "localhost"
  private state: Promise<boolean>
  private socket: WebSocket

  constructor(port = "15421") {
    super()

    this.port = port;

    let socket = new WebSocket(`ws://${this.host}:${this.port}`);
    this.socket = socket;

    this.state = new Promise((res, rej) => {
      socket.onopen = msg => {
        socket.onerror = null;
        res(true);

        socket.addEventListener("close", msg => {
          console.log("Connect closed!");
        })

        socket.addEventListener("err", msg => {
          console.error("WS Error", msg);
        })

        socket.addEventListener("message", msg => {
          console.log("Message received");
        })
      }

      socket.onerror = msg => {
        rej(false)
      }
    });
  }

  async is_valid(): Promise<boolean> {
    try {
      return await this.state;
    } catch (e) {
      return false;
    }
  }

  async build_grammar(grammar_string: string, config: JSParserConfig) {
    if (await this.state) {
      let encoder = new TextEncoder;
      let min_len = (grammar_string.length * 2.5) | 0;
      let config_buffer = new Uint8Array(config.serialize());
      let config_buffer_size = config_buffer.byteLength;

      let data = new Uint8Array(8 + config_buffer_size + min_len);

      console.log({ config_buffer_size, size: JSParserConfig.size() })

      let transfer = new Uint8Array(data.buffer, 8, config_buffer_size);

      config_buffer.forEach((v, i) => transfer[i] = v)

      console.log(transfer);

      let { read, written } = encoder.encodeInto(grammar_string, data.subarray(8 + config_buffer_size));

      if (read < grammar_string.length) {
        throw "Did not read all grammar bytes";
      }

      let dv = new DataView(data.buffer);

      dv.setUint8(0, WSRequestCodes.BUILD_GRAMMAR);
      dv.setUint32(4, written, true);

      let buffer = data.buffer.slice(0, 8 + config_buffer_size + written);

      this.socket.send(buffer);
    }
  }
}


export function createConnection(): RadlrWSClient {
  return new RadlrWSClient();
}