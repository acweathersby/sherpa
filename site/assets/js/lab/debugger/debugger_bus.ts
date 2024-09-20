import { JSByteCodeParser, JSBytecodeParserDB, JSCTXState, JSDebugPacket, JSIRParser, JSParserDB } from "js/radlr/radlr_wasm";

class DebuggerMessageBus {

  listeners = new Map();

  addListener<T extends keyof DebuggerMessage>(type: T, listener: (msg: DebuggerMessage[T]) => void) {
    if (!this.listeners.get(type)) {
      this.listeners.set(type, new Set);
      this.addListener(type, listener);
    } else {
      this.listeners.get(type).add(listener);
    }
  }

  announce<T extends keyof DebuggerMessage>(msg: DebuggerMessage[T]) {
    let type = msg.type;
    let listeners = this.listeners.get(type);
    if (listeners) {
      for (const listener of listeners.values()) {
        listener(msg);
      }
    }
  }
}

export const debugger_bus = new DebuggerMessageBus();


type DebuggerMessageBase<T extends keyof DebuggerMessage> = {
  type: T;
}

export type DebuggerMessage = {
  "error": DebuggerMessageBase<"error"> & {
    msg: string
  }
  "db-deleted": DebuggerMessageBase<"db-deleted"> & {}
  "db-created": DebuggerMessageBase<"db-created"> & {
    db: JSParserDB
  }
  "clear-error": DebuggerMessageBase<"clear-error"> & {}
  "parser-db-created": DebuggerMessageBase<"parser-db-created"> & {
    bc_db: JSBytecodeParserDB
  },
  "states-created": DebuggerMessageBase<"states-created"> & {
    states: JSIRParser,
  },
  "parser-created": DebuggerMessageBase<"parser-created"> & {
    parser: JSByteCodeParser
  },
  "parser-reset": DebuggerMessageBase<"parser-reset"> & {
    parser: JSByteCodeParser
  },
  "step": DebuggerMessageBase<"step"> & {
    key_frame: boolean,
    debug_data: JSDebugPacket,
    parser: JSByteCodeParser,
    bc_db: JSBytecodeParserDB
    db: JSParserDB,
    states: JSIRParser
    input: string
  }
}



