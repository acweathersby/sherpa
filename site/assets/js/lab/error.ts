import { ErrorOrigin, JSRadlrSourceError } from "js/radlr/radlr_wasm";

export class RadlrError {
  col: number = 0;
  line: number = 0;

  len: number = 0;
  start_offset: number = 0;
  end_offset: number = 0;

  msg: string = "";
  origin: ErrorOrigin = ErrorOrigin.BytecodeImport

  constructor(err: JSRadlrSourceError | undefined) {
    if(err) {

      this.col = err.col;
      this.line = err.line;
      
      this.len = err.len;
      this.start_offset = err.start_offset;
      this.end_offset = err.end_offset;
      
      this.msg = err.message;
      this.origin = err.origin;
      err.free
    }
  }
}