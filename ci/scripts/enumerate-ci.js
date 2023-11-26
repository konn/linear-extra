module.exports = async ({ github, context, core, glob, io, require }) => {
  const fs = require("fs");
  const globber = await glob.create("ci/configs/*.project");
  const globber_falliable = await glob.create("ci/configs/head/*.project");
  const files = await globber.glob();
  const files_falliable = await globber_falliable.glob();

  function generate_obj(fullpath) {
    const is_head = fullpath.test(/configs\/head/);
    const prefix = is_head ? "ci/configs/head/" : "ci/configs/";
    const path = fullpath.replace(/^.+\//, prefix);
    const name = fullpath.replaceAll(/.+\/|\.project$/g, "");
    const match = fs
      .readFileSync(fullpath, "utf-8")
      .match(/with-compiler:\s*ghc-([\d\.]+)/);
    const ghc = match[1];

    return { path, name, ghc, is_head };
  }

  const plans = files.map((fullpath) => {
    const path = fullpath.replace(/^.+\//, "ci/configs/");
    const name = fullpath.replaceAll(/.+\/|\.project$/g, "");
    const match = fs
      .readFileSync(fullpath, "utf-8")
      .match(/with-compiler:\s*ghc-([\d\.]+)/);
    const ghc = match[1];

    return { path, name, ghc };
  });
  core.info(`plan: ${JSON.stringify(plans)}`);
  core.setOutput("plan", JSON.stringify(plans));
};
