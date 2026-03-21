# setup_BASEHANK.jl
#
# Downloads the BASEforHANK toolbox (v1.5.0) into this directory.
# Run this ONCE before using the Julia environment:
#
#   julia setup_BASEHANK.jl
#
# Then install Julia dependencies as usual:
#
#   julia --project=. -e "using Pkg; Pkg.instantiate()"

const REPO_URL  = "https://github.com/BASEforHANK/BASEtoolbox.jl"
const TAG       = "v1.5.0"
const DIRS      = ["src", "examples", "docs"]

if isfile(joinpath(@__DIR__, "src", "BASEforHANK.jl"))
    println("BASEforHANK toolbox already present — nothing to do.")
    exit()
end

println("Downloading BASEforHANK toolbox $TAG from GitHub...")

tmp = mktempdir()
try
    run(`git clone --depth 1 --branch $TAG $REPO_URL $tmp`)
    for dir in DIRS
        src = joinpath(tmp, dir)
        isdir(src) || continue
        cp(src, joinpath(@__DIR__, dir); force = true)
        println("  copied $dir/")
    end
    println("\nDone. Now run:\n\n  julia --project=. -e \"using Pkg; Pkg.instantiate()\"\n")
finally
    rm(tmp; recursive = true, force = true)
end
