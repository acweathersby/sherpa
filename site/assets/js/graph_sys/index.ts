class TextureAtlasGenerator {
  ctx: OffscreenCanvasRenderingContext2D
  canvas: OffscreenCanvas


  constructor() {
    this.canvas = document.createElement("canvas").transferControlToOffscreen();
    let ctx = this.canvas.getContext("2d");

    if (!ctx) {
      throw "Context 2d Not Supported!";
    }

    this.ctx = ctx;
  }
}

export class SyntaxGraphEngine {
  gl: WebGL2RenderingContext
  canvas: HTMLCanvasElement

  nodes: { pos: { x: number, y: number } }[]
  connections: number[]

  line_buffer: WebGLBuffer
  node_buffer: WebGLBuffer

  line_program: WebGLProgram
  node_program: WebGLProgram

  node_count: number = 0
  line_count: number = 0

  line_vao: WebGLVertexArrayObject
  node_vao: WebGLVertexArrayObject

  camera_uniform: WebGLUniformLocation

  transfer_buffer: ArrayBuffer = new ArrayBuffer(1024 * 4);

  /// Establishes a node graph engine within the provided HTMLElement. The
  /// context will always take up the entire space available within the ele argument.
  constructor(par_ele: HTMLDivElement) {
    this.canvas = document.createElement("canvas");
    let gl = this.canvas.getContext("webgl2");

    if (!gl) {
      throw "Webgl 2 Not Supported!";
    }

    par_ele.appendChild(this.canvas);
    this.canvas.classList.add("syntax-engine");

    this.gl = gl;

    this.nodes = []
    this.connections = []

    this.node_program = createProgram(gl, circle_draw_vert, circle_draw_frag);
    this.line_program = createProgram(gl, line_draw_vert, line_draw_frag);

    this.line_buffer = createBuffer(gl);
    this.node_buffer = createBuffer(gl);
    
    this.line_vao = createVAO(gl);
    this.node_vao = createVAO(gl);
    


    this.camera_uniform = createBuffer(gl);


    //gl.bindBuffer(gl.UNIFORM_BUFFER, this.camera_uniform);
    gl.bindBufferBase(gl.UNIFORM_BUFFER, 1, this.camera_uniform); 
    //gl.bindBuffer(gl.UNIFORM_BUFFER, null);

    let block_index = gl.getUniformBlockIndex(this.node_program, "UIData"); 
    gl.uniformBlockBinding(this.node_program, block_index, 1);

    block_index = gl.getUniformBlockIndex(this.line_program, "UIData"); 
    gl.uniformBlockBinding(this.line_program, block_index, 1);

    this.setupGL();

    gl.bindBuffer(gl.UNIFORM_BUFFER, this.camera_uniform);
    gl.bufferData(gl.UNIFORM_BUFFER, new Float32Array([0, 0, 0, 0, 0.2, 0, 0, 0]), gl.DYNAMIC_DRAW);
    gl.bindBuffer(gl.UNIFORM_BUFFER, null);

    let i = 0;

    let draw_fn = () => {
      this.clearNodes();

      let a = this.addNode(0, 0.0);
      let b = this.addNode(Math.sin(i) * 500, 600);
      let c = this.addNode(Math.cos(i) * Math.sin(-i) * 500, 200);
      let d = this.addNode(Math.cos(i) * 500, 300);

      for(let i = 0; i < 1000; i++){
        this.addNode(Math.random() * 4000 - 2000, Math.random() * 4000 - 2000)
      }
  
      this.addConnection(a,b);
      this.addConnection(b,c);
      this.addConnection(c,d);
      this.addConnection(a,d);
      
      this.uploadLines();
      this.uploadNodes();

      i+= Math.PI * .005
      this.draw();
      //return
      requestAnimationFrame(draw_fn)
    };
    requestAnimationFrame(draw_fn);
  }


  addNode(x: number, y: number) : number {
    let index = this.nodes.length;
    this.nodes.push({pos: { x, y }})
    return index
  }

  clearNodes() {
    this.nodes.length = 0;
    this.connections.length = 0;
  }

  addConnection(node_a: number, node_b: number){
    
    this.connections.push(node_a, node_b);
  }

  private draw() {
    let { gl, node_program, line_program, node_buffer, line_buffer } = this;
  
    let { width, height } = this.canvas.getBoundingClientRect();
    this.canvas.width = width;
    this.canvas.height = height;
    gl.viewport(0, 0, width, height);

    gl.bindBuffer(gl.UNIFORM_BUFFER, this.camera_uniform);
    gl.bufferData(gl.UNIFORM_BUFFER, new Float32Array([width, height, 0, 0, 0.2, 0, 0, 0]), gl.DYNAMIC_DRAW);
    gl.bindBuffer(gl.UNIFORM_BUFFER, null);


    gl.clearColor(0.0, 0.0, 0.0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT);

    gl.enable(gl.BLEND);
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA);

    gl.bindVertexArray(this.line_vao);
    gl.useProgram(line_program);
    gl.drawArraysInstanced(gl.TRIANGLES, 0, 6, this.line_count);

    
    gl.bindVertexArray(this.node_vao);
    gl.useProgram(node_program);

    gl.drawArraysInstanced(gl.TRIANGLES, 0, 6, this.node_count);
      // Create base line shader for drawing circles
      let error = 0;
      if (error = gl.getError()) {
        gl.INVALID_ENUM
        gl.INVALID_OPERATION
        console.log({error});
        throw "Have gl errors"
      }
    error = gl.getError()
  }

  private uploadNodes() {

    let required_buffer_size = this.nodes.length * 3 * 4;

    if(required_buffer_size > this.transfer_buffer.byteLength) {
      this.transfer_buffer = new ArrayBuffer(required_buffer_size);
      console.log("aa")
    } else if(required_buffer_size == 0) { 
      return
    }

    let nodes_f32 = new Float32Array(this.transfer_buffer, 0, this.nodes.length * 3);

    this.node_count = this.nodes.length;

    for(let i = 0, l = this.nodes.length; i < l; i++) {
      let node = this.nodes[i];
      nodes_f32[i * 3 + 0] = node.pos.x
      nodes_f32[i * 3 + 1] = node.pos.y 
      nodes_f32[i * 3 + 2] = 0
    }

    let { gl, node_buffer } = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, node_buffer);
    gl.bufferData(gl.ARRAY_BUFFER, nodes_f32, gl.DYNAMIC_DRAW);
    gl.bindBuffer(gl.ARRAY_BUFFER, null);
  }

  private uploadLines() {

    let required_buffer_size = this.connections.length * 3 * 4;

    if(required_buffer_size > this.transfer_buffer.byteLength) {
      this.transfer_buffer = new ArrayBuffer(required_buffer_size);
      console.log("aa")
    } else if(required_buffer_size == 0) {
      return
    }

    let line_data = new Float32Array(this.transfer_buffer, 0, this.connections.length * 3);
    
    this.line_count = this.connections.length / 2;

    for(let i = 0, l = this.connections.length; i < l; i+=2) {
      let node_a = this.nodes[this.connections[i]];
      let node_b = this.nodes[this.connections[i + 1]];

      line_data[i * 3 + 0] = node_a.pos.x;
      line_data[i * 3 + 1] = node_a.pos.y;
      line_data[i * 3 + 2] = node_b.pos.x;
      line_data[i * 3 + 3] = node_b.pos.y;
      line_data[i * 3 + 4] = 5;
      line_data[i * 3 + 5] = 5;
    }

    let { gl, line_buffer } = this;
    gl.bindBuffer(gl.ARRAY_BUFFER, line_buffer);
    gl.bufferData(gl.ARRAY_BUFFER, line_data, gl.DYNAMIC_DRAW);
    gl.bindBuffer(gl.ARRAY_BUFFER, null);
  }

  private setupGL() {
    let { gl, node_program, line_program, node_buffer, line_buffer, line_vao, node_vao } = this;

    {
      // Setup 

      // gl.useProgram(line_program);

      
      gl.bindVertexArray(line_vao);

      gl.bindBuffer(gl.ARRAY_BUFFER, line_buffer);

      var p1_attr = gl.getAttribLocation(line_program, "point1");
      gl.enableVertexAttribArray(p1_attr);
      gl.vertexAttribPointer(p1_attr, 2, gl.FLOAT, false, 6 * 4, 0);
      gl.vertexAttribDivisor(p1_attr, 1);

      var p2_attr = gl.getAttribLocation(line_program, "point2");
      gl.enableVertexAttribArray(p2_attr);
      gl.vertexAttribPointer(p2_attr, 2, gl.FLOAT, false, 6 * 4, 2 * 4);
      gl.vertexAttribDivisor(p2_attr, 1);

      var width_attr = gl.getAttribLocation(line_program, "width");
      gl.enableVertexAttribArray(width_attr);
      gl.vertexAttribPointer(width_attr, 2, gl.FLOAT, false, 6 * 4, 4 * 4);
      gl.vertexAttribDivisor(width_attr, 1);
      gl.bindBuffer(gl.ARRAY_BUFFER, null);

      
      gl.bindVertexArray(node_vao);

      // Create a node buffer to store node positions
      gl.bindBuffer(gl.ARRAY_BUFFER, node_buffer);
      var pos_attrib = gl.getAttribLocation(node_program, "pos");
      gl.enableVertexAttribArray(pos_attrib);
      gl.vertexAttribPointer(pos_attrib, 3, gl.FLOAT, false, 0, 0);
      gl.vertexAttribDivisor(pos_attrib, 1);
      gl.bindBuffer(gl.ARRAY_BUFFER, null);

      console.log({pos_attrib, p1_attr})

    }

    // Create base line shader for drawing circles
    if (gl.getError()) {
      throw "Have gl errors"
    }
  }
}

function createVAO(gl: WebGL2RenderingContext) {
  let line_vao = gl.createVertexArray();
  if (!line_vao) throw "Could not create VAO";
  return line_vao;
}

function createBuffer(gl: WebGL2RenderingContext) {
  let buffer = gl.createBuffer();
  if (!buffer) { throw "Could not create GL buffer"; }
  return buffer;
}

function createProgram(gl: WebGL2RenderingContext, vert_shader_source: string, frag_shader_source: string) {
  var program = gl.createProgram();

  if (!program) { throw "Could not create shader"; }

  let vert_shader = gl.createShader(gl.VERTEX_SHADER);
  if (!vert_shader) { throw "Could not create shader"; }
  gl.shaderSource(vert_shader, vert_shader_source);
  gl.compileShader(vert_shader);

  let vert_log = gl.getShaderInfoLog(vert_shader);
  if (vert_log) {
    console.error(vert_log);
    throw "Could not compile program: Vert Error";
  }

  let frag_shader = gl.createShader(gl.FRAGMENT_SHADER);
  if (!frag_shader) { throw "Could not create shader"; }
  gl.shaderSource(frag_shader, frag_shader_source);
  gl.compileShader(frag_shader);

  let frag_log = gl.getShaderInfoLog(frag_shader);
  if (frag_log) {
    console.error(frag_log);
    throw "Could not compile program: Frag ERror";
  }

  gl.attachShader(program, vert_shader);
  gl.attachShader(program, frag_shader);



  gl.linkProgram(program);

  let program_log = gl.getProgramInfoLog(program);
  if (program_log) {
    console.error(program_log);
    throw "Could not compile program";
  }

  return program;
}



let ui_block = `

uniform UIData {
  vec2 screen_size;
  vec2 mouse_pos;
  vec2 screen_offset;
};


`

let static_square2 = `
#define right 1.0
#define left -1.0
#define top 1.0
#define bottom -1.0

#define top_left vec2(left, top)
#define top_right vec2(right, top)

#define bottom_left vec2(left, bottom)
#define bottom_right vec2(right, bottom)

vec2[6] positions = vec2[6]( 
  // 0 - 2
  top_left,
  top_right,
  bottom_left,
  // 3 - 5
  top_right,
  bottom_right,
  bottom_left
);
`

let line_draw_vert = `#version 300 es

in vec2 point1;
in vec2 point2;
in vec2 width;

${static_square2}

${ui_block}

void main(){
  vec2 static_pos = positions[gl_VertexID] ;

  vec2 diff = point2 - point1 ;

  float theta = atan(diff.y, diff.x);

  float _sin = sin(-theta);
  float _cos = cos(-theta);

  mat2 rot =  mat2( _cos, -_sin, _sin, _cos);

  vec2 scaled =  static_pos * vec2(length(diff) * 0.5, width.x * 0.5) ;

  vec2 rot_pos = rot * scaled ;

  vec2 offset = (point1 + diff * 0.5);

  gl_Position = vec4((rot_pos  + offset) / screen_size + screen_offset, 0.0, 1);
}
`;

let line_draw_frag = `#version 300 es

precision highp float;
out vec4 color;
void main(){
  color = vec4(1, 0, 0, 1);
}
`

let circle_draw_vert = `#version 300 es

${static_square2}

in vec3 pos;

${ui_block}

out vec3 base_color; 
flat out vec2 center_point;
smooth out vec2 actual_point; 

float diameter = 40.0;

void main(){
  vec2 static_pos = positions[gl_VertexID];
 
  actual_point = static_pos;
  center_point = vec2(0, 0);

  vec2 adjusted_pos = (static_pos * diameter + pos.xy) / screen_size + screen_offset;

  gl_Position = vec4(adjusted_pos, 0.0, 1);
  
  if(gl_VertexID > 2) {
    base_color = vec3(0,1,1);
  } else {
    base_color = vec3(0,1,1);
  }
}
`

let circle_draw_frag = `#version 300 es

precision highp float;


in vec3 base_color;
flat in vec2 center_point; 
smooth in vec2 actual_point; 

out vec4 color;

void main(){


  float val = length(actual_point - center_point) * 1.0;

  val = smoothstep(0.90, 1.0, val);

  if(val > 1.0)
    discard;

  color = vec4(base_color, 1.0 -val);
}
`;