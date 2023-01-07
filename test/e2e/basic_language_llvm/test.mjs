import fs from "fs";

let data = await fs.promises.readFile("./spirv.core.grammar.json", { encoding: "utf8" });

let time = performance.now();

data = JSON.parse(data);

let end = performance.now();

let elapsed = end - time;

console.log(data);

console.log(elapsed);