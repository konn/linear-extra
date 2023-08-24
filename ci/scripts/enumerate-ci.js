module.exports = async ({ github, context, core, glob, io, require }) => {
  const fs = require("fs");
  const globber = await glob.create("ci/configs/*.project");
  const files = await globber.glob();
  const plans = files.map((path) => {
    const name = path.replaceAll(/.+\/|\.project$/g, "");
    const src = fs.readFileSync(path);
    const match = src.match(/with-compiler:\s*ghc-([\d\.]+)/);
    const ghc = match[1];

    return { path, name, ghc };
  });
  core.setOutput("plan", JSON.stringify(plans));
};
