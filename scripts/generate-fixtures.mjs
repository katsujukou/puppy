// Regenerates the parsers under fixture/src from the grammars beside them.
//
// Run after `spago build`, since it drives the generator out of `output/`.
import { readFileSync, writeFileSync } from "node:fs";

const R = new URL("../output/", import.meta.url).pathname;
const P = await import(`${R}Puppy.Syntax.Parser/index.js`);
const E = await import(`${R}Puppy.Expand/index.js`);
const G = await import(`${R}Puppy.LR.Grammar/index.js`);
const An = await import(`${R}Puppy.LR.Analysis/index.js`);
const A = await import(`${R}Puppy.LR.Automaton/index.js`);
const T = await import(`${R}Puppy.LR.Table/index.js`);
const X = await import(`${R}Puppy.LR.Explain/index.js`);
const C = await import(`${R}Puppy.Codegen/index.js`);

const root = new URL("../", import.meta.url).pathname;

const fixtures = [
  { grammar: "calculator.puppy", module: "Puppy.Fixture.Calculator" },
  { grammar: "lists.puppy", module: "Puppy.Fixture.Lists" },
  { grammar: "awkward.puppy", module: "Puppy.Fixture.Awkward" },
];

let failed = false;
for (const { grammar, module: moduleName } of fixtures) {
  const src = readFileSync(`${root}fixture/grammars/${grammar}`, "utf8");
  const syn = P.parse(src);
  if (syn.constructor.name === "Left") {
    console.error(`${grammar}: parse: ${syn.value0.message}`);
    failed = true;
    continue;
  }
  const core = E.expand(syn.value0);
  if (core.constructor.name === "Left") {
    console.error(`${grammar}: ${core.value0.map((e) => e.message).join("; ")}`);
    failed = true;
    continue;
  }
  const g = G.number(core.value0);
  if (g.constructor.name === "Left") {
    console.error(`${grammar}: ${g.value0.map((e) => e.message).join("; ")}`);
    failed = true;
    continue;
  }
  const aut = A.build(A.Pager.value)(g.value0)(An.analyse(g.value0));
  if (aut.constructor.name === "Left") {
    console.error(`${grammar}: automaton: ${aut.value0.map((e) => e.message).join("; ")}`);
    failed = true;
    continue;
  }
  const table = T.tabulate(g.value0)(aut.value0);
  const reports = X.report(g.value0)(aut.value0)(table);
  if (reports.length) {
    console.error(`${grammar}: ${reports.length} unresolved conflict(s)`);
    reports.forEach((r) => console.error(r));
    failed = true;
    continue;
  }
  const out = C.generate({
    moduleName, core: core.value0, grammar: g.value0, automaton: aut.value0, table,
  });
  if (out.constructor.name === "Left") {
    console.error(`${grammar}: codegen: ${out.value0}`);
    failed = true;
    continue;
  }
  const path = `${root}fixture/src/${moduleName.replace(/\./g, "/")}.purs`;
  writeFileSync(path, out.value0.source);
  console.log(`${grammar} -> ${moduleName} (${out.value0.mappings.length} mappings)`);
}
process.exit(failed ? 1 : 0);
