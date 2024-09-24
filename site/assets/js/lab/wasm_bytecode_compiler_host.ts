import { JSParserConfig } from "js/radlr/radlr_wasm";

type WorkerEvents = {
  "error": null,
  "grammar_compiled": null,
  "parser_compiled": ArrayBuffer
}

export class WasmBytecodeCompiler {
  listeners: Map<any, ((arg: any) => void)[]> = new Map;
  worker: Worker
  _ready: Promise<boolean>



  constructor(worker_path: string) {
    // Create an object to later interact with 
    const proxy = {};

    // Keep track of the messages being sent
    // so we can resolve them correctly
    let id = 0;
    let idPromises = {};


    const worker = new Worker(worker_path, { type: "module" });
    this.worker = worker;


    worker.addEventListener('message', (event) => {
      if (event.data.type == "parser_compiled") {
        this.emit("parser_compiled", event.data.bytecode_db_export)
      }
    });



    worker.addEventListener("error", function (error) {
      console.log(error)
    });

    this._ready = new Promise((res, rej) => {
      worker.addEventListener('message', function (event) {
        console.log(event.data.type)
        if (event.data.type == "ready") {
          res(true);
        }
      });
    });

    this._ready.catch(err => {
      console.log(err)
    })

    worker.postMessage({ type: "init", eventData: void 0 });
  }

  // Compiles a grammar file, producing a grammar_db which is stored in the worker, and a bytecode
  // parser that is transferred through the `parser_generated`  event, with data that should be imported
  // into the host radlr instance.
  compileGrammar(grammar: string, config: JSParserConfig) {
    this.worker.postMessage({ type: "compile_grammar", eventData: { grammar, config }, })
  }


  async ready(): Promise<boolean> {
    return this._ready
  }

  protected emit<T extends keyof WorkerEvents, A = WorkerEvents[T], D = (arg: A) => void>(event: T, data: A) {
    for (const listener of this.listeners.get(event) ?? []) {
      (listener)(<any>data)
    }
  }

  addListener<T extends keyof WorkerEvents, A = WorkerEvents[T], D = (arg: A) => void>(event: T, listener: D) {
    if (!this.listeners.get(event)) {
      this.listeners.set(event, [<any>listener]);
    } else {
      this.listeners.get(event)?.push(<any>listener);
    }
  }

  removeListener<T extends keyof WorkerEvents, A = WorkerEvents[T], D = (arg: A) => void>(event: T, listener: D) {
    let listeners = this.listeners.get(event);
    if (listeners) {
      let index = listeners.findIndex(<any>listener);
      if (index > 0) {
        listeners.splice(index, 1)
      }
    }
  }

}