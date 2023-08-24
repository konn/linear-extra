module.exports = async ({ github, context, core, glob, io, require }) => {
  const fs = require("fs");
  const globber = await glob.create("ci/configs/*.project");
  const files = await globber.glob();
  const plans = files.map((path) => {
    const name = path.replaceAll(/.+\/|\.project$/g, "");
    const match = fs
      .readFileSync(path, "utf-8")
      .match(/with-compiler:\s*ghc-([\d\.]+)/);
    const ghc = match[1];

    return { path, name, ghc };
  });
  core.info(`plan: ${JSON.stringify(plans)}`);
  core.setOutput("plan", JSON.stringify(plans));
};
