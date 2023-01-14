// This script should be run by deno
type Lock = {
  nodes: NodeMap
  root: string
  version: number
}

type NodeMap = { [key: string]: Node }

type Node = {
  flake?: boolean
  locked?: Source
  original?: Source
  inputs?: { [key: string]: string}
}

type Source = {
  lastModified?: Number
  revCount?: number
  narHash?: string
  owner?: string
  repo?: string
  url?: string
  path?: string
  rev?: string
  type?: string
  ref?: string
}

///////////////////
// Read the data //
///////////////////

const { nodes } = JSON.parse(await Deno.readTextFile('flake.lock'))

const entries = Object.entries(nodes)
  .map(x => [x[0], x[1]?.original?.owner])
  .filter(x => !!x[1]) as string[][]

const owners = new Set<string>()

for (const x of entries) {
  owners.add(x[1] as string)
}

const gitEntries = Object.entries(nodes)
  .filter(x => x[1].type === "git")
  .map(x => x[0])

//////////////////
// Print tables //
//////////////////

const table: [string, number, string][] = []

for (const owner of owners.values()) {
  if (owner === "akirak")
    continue
  const names = entries.filter((x: string[]) => x[1] === owner).map(x => x[0])
  if (names.length > 1) {
    table.push([owner, names.length, names.join(",")])
  }
}

console.table(table.sort((x, y) => y[1] - x[1]))

//////////////////////////
// Update the lock file //
//////////////////////////

async function updateFlakeInputs(inputs: string[]): Promise<boolean> {
  const p = Deno.run({ stderr: 'piped', cmd: ["nix", "flake", "lock"].concat(
    inputs.map(input => ["--update-input", input]).flat()
  )});
  await p.status()
}

async function gitDiffFile(file: string): Promise<boolean> {
  const p = Deno.run({ cmd: ["git", "diff", "--quiet", "--", file]})
  const { code } = await p.status()
  return code == 1
}

async function maybeGitCommit(message: string): Promise<void> {
  const updated = await gitDiffFile("flake.lock")
  if (updated) {
    const p = Deno.run({cmd: [ "git", "commit", "-m", message, "--no-verify", "flake.lock" ]})
    await p.status()
  }
}

for (const owner of owners) {
  const inputs = entries.filter((x: string[]) => x[1] === owner).map(x => x[0])

  await updateFlakeInputs(inputs)
  await maybeGitCommit(
    (inputs.length == 1) ?
      `emacs: Update ${inputs[0]}` :
      `emacs: Update packages by ${owner}`
  )
}

for (const name of gitEntries) {
  await updateFlakeInputs([name])
  await maybeGitCommit(`emacs: Update ${name}`)
}
