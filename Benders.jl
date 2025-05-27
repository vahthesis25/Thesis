#Viktor Andri Henriksen

using Random, Distributions
using JuMP
using Gurobi
using DataFrames
using XLSX
using CSV
using Dates
using Clustering
using Distances
import Printf
Random.seed!(1)


export Port, Route, Vessel

#=
 Define data structures for ports and vessels
=#
struct Port
    nodeIndex::Int64
    UNLOCODE::Symbol
    name::Symbol
    max_draft::Int64
    type::Int64 #Bunker, Cargo, Both
    region::Symbol
    min_bunker::Float64
    time_quantity::Float64
    bunker_price::Tuple{Int64, Int64}
    agency_fee::Int64
end

struct Route
    routeIdx::Int64
    or::Port 
    dest::Port
    ID::Symbol #1=Skaw, 2=Kiel_canal, etc 
    nLegs::Int64 # How many legs are on the route
    loadline::Vector{Symbol} #loadline for each leg...  1=summer, 2=winter, 3=tropical, Maybe change to symbol
    fuel_type::Vector{Symbol} #fuel type for each leg... 1=MGO, 2=VLSFO
    distance::Vector{Int64} #distance of each leg
    distToID::Int64
end

struct Vessel
    id::Int64
    type::Symbol
    max_cargo::Vector{Int64} # summer,winter,tropical
    full_laden_draft::Vector{Int64} # draft line at max cargo for summer,winter,tropical
    ton_per_meter::Float64 # conversion
    bunker_cap::Array{Float64, 2} # lower,upper limit for each fuel type
    speed::Int64 #This is constant so maybe not necessary here? knt/h
    consumption::Int64 # fuel consumption mts per day
end

struct myGraph
    nodes::Vector{Port}
    arcs::Vector{Route}
    portSets::Array{Vector{Int64}}
    incomingArcs::Dict{Port, Vector{Int64}} #links port nodes to Arc index in arcs
    outgoingArcs::Dict{Port, Vector{Int64}} #links port nodes to Arc index in arcs
    start::Port
    finish::Port
end

#=
Load data into dataframes.
=#
function load_data(file_path)
    # Load the Excel file
    data = XLSX.readxlsx(file_path)
    names = XLSX.sheetnames(data)
    df = Dict()
    for name in names
        df[name] = DataFrame(XLSX.readtable("C:/Users/vikto/Documents/DTU/Thesis/GTProutes.xlsx",name))
    end

    #PORT DATA
    dfPorts = leftjoin(df["PortRegion"], df["PortRestriction"], on = :UNLOCODE)
    dfPorts = leftjoin(dfPorts, df["PortType"], on = :UNLOCODE)
    # Add boolean columns based on the 'Type' column
    dfPorts[!, :BunkerPort] = dfPorts.Type .== "Bunker_port"
    dfPorts[!, :CargoPort] = dfPorts.Type .== "Cargo_port"

    dfPorts = combine(groupby(dfPorts, [:UNLOCODE, :Port, :Region, :LadenMaxDraft]), 
                    :BunkerPort => maximum => :BunkerPort, 
                    :CargoPort => maximum => :CargoPort)

    dfPorts = leftjoin(dfPorts, df["BunkerPortTime"], on = :UNLOCODE)
    dfPorts = leftjoin(dfPorts, df["BunkerPortAgencyCost"], on = :UNLOCODE)
    dfPorts = leftjoin(dfPorts, unstack(df["BunkerPortPrices"][:, [:UNLOCODE, :BunkerGrade, :Price]], :BunkerGrade, :Price), on = :UNLOCODE )

    #ROUTE DATA
    dfRoute = df["Distances"]
    dfRoute = combine(groupby(dfRoute, [:UNLOCODE_From, :UNLOCODE_To, :RoutingID]),
                    :Sequence => maximum,
                    :Loadline => x -> push!([], x),  # Build Loadline as a vector
                    :FuelType => x -> push!([], x),  # Build FuelType as a vector
                    :DistanceNM => x -> push!([], x),  # Build DistanceNM as a vector
                    :DistToRoutingID => maximum
    )

    routeRestriction = Dict(Symbol(row[:RoutingID]) => (row[:MaxDwt], row[:DraftLaden]) for row in eachrow(df["RoutingRestriction"]))

    #VESSEL DATA


    #OTHER DATA to dictionaries
    seaMargin = Dict()
    for i in 1:nrow(df["SeaMargin"])
        seaMargin[Symbol(df["SeaMargin"].Region_from[i]), Symbol(df["SeaMargin"].Region_to[i])] = (1 + df["SeaMargin"].Seamargin[i])
        seaMargin[Symbol(df["SeaMargin"].Region_to[i]), Symbol(df["SeaMargin"].Region_from[i])] = (1 + df["SeaMargin"].Seamargin[i])
    end
    #hireCost = 

    #freightRate

    #routingRestriction =

    #fixedRoutingCost=

    return dfPorts, dfRoute, seaMargin, routeRestriction
end

#=
Check whether a route is needed when optimizing route between start and finish
Input:
    or: origin of route to Check
    dest: destionation of route to Check
    start: Port of origin of the route to optimize
    finish: Destination port of the route to optimize
Output:
    Boolean indicating if route between or and dest is needed.
=#
function isfeasible(or::Port, dest::Port, start::Port, finish::Port)
    isStart = (or == start) && (dest.type == 1 || dest == finish)
    isEnd = (or.type == 1) && (dest == finish)
    Bunker2Bunker = (or.type == 1) && (dest.type == 1)
    return (isStart || isEnd || Bunker2Bunker)
end

#=
Put data from load_data into structs and create graph for optimization model.
=#
function createInstance()#start,finish)
    dfPorts, dfRoute, seaMargin, rr = load_data("C:/Users/vikto/Documents/DTU/Thesis/GTProutes.xlsx")

    Ports = Port[]

    nrPorts = size(dfPorts,1) 
    nrRoutes = size(dfRoute,1)

    UNLOCODEtoPort = Dict{Symbol, Port}()

    bunkerPorts = []
    cargoPorts = []
    bothPorts = []

    for i=1:nrPorts
        code = Symbol(dfPorts[i,1])
        name = Symbol(dfPorts[i,2])
        maxDraft = Int64(dfPorts[i,4])
        type = (dfPorts[i,5] .== true .&& dfPorts[i,6] .== false) .+ 2 * (dfPorts[i,5] .== false .&& dfPorts[i,6] .== true) .+ 3 * (dfPorts[i,5] .== true .&& dfPorts[i,6] .== true)
        region = Symbol(dfPorts[i,3])
        minBunker = ismissing(dfPorts[i, 10]) ? 0.0 : Float64(dfPorts[i, 10])
        timePerQuant = ismissing(dfPorts[i, 11]) ? 0.0 : Float64(dfPorts[i,11])
        bunkerPrice = (ismissing(dfPorts[i, 13]) ? 0 : Int64(dfPorts[i,13]), ismissing(dfPorts[i, 14]) ? 0 : Int64(dfPorts[i,14]))
        agFee = ismissing(dfPorts[i, 12]) ? 0 : Int64(dfPorts[i,12])
        port = Port(i, code, name, maxDraft, type, region, minBunker, timePerQuant, bunkerPrice, agFee)
        push!(Ports, port)
        UNLOCODEtoPort[code] = port
        if type == 1
            push!(bunkerPorts,i)
        elseif type == 2 
            push!(cargoPorts, i)
        elseif type == 3 
            push!(bothPorts, i)
        end
    end

    portSets = [bunkerPorts, cargoPorts, bothPorts]

    start = UNLOCODEtoPort[Symbol(:BRSSZ)]
    finish = UNLOCODEtoPort[Symbol(:PLGND)]

    incomingArcs = Dict{Port, Vector{Int64}}()#Map of Port to incoming route ids (index in Arcs array)
    outgoingArcs = Dict{Port, Vector{Int64}}()#Map of Port to outgoing route ids (index in Arcs array)
    Routes = Route[]

    for port in Ports
        incomingArcs[port] = Int64[] # Initialize empty vector for incoming routes
        outgoingArcs[port] = Int64[] # Initialize empty vector for outgoing routes
    end
    
    routeIdx = 1
    for i in 1:nrRoutes 
        or = UNLOCODEtoPort[Symbol(dfRoute[i,1])]
        dest = UNLOCODEtoPort[Symbol(dfRoute[i,2])]
        id = Symbol(dfRoute[i,3])
        legs = Int64(dfRoute[i,4])
        loadline = Symbol.(dfRoute[i,5]) 
        fuel = Symbol.(dfRoute[i,6])
        dist = convert(Vector{Int64}, dfRoute[i,7])
        distToID = ismissing(dfRoute[i, 8]) ? 0 : Int64(dfRoute[i,8])
        if isfeasible(or, dest, start, finish)
            route1 = Route(routeIdx, or, dest, id, legs, loadline, fuel, dist, distToID)
            push!(Routes, route1)
            push!(outgoingArcs[or], routeIdx)
            push!(incomingArcs[dest], routeIdx)
            routeIdx = routeIdx +1
        end
        if isfeasible(dest, or, start, finish)
            route2 = Route(routeIdx, dest, or, id, legs, reverse(loadline), reverse(fuel), reverse(dist), ismissing(dfRoute[i, 8]) ? 0 : (sum(dist) - distToID))
            push!(Routes, route2)
            push!(outgoingArcs[dest], routeIdx)
            push!(incomingArcs[or], routeIdx)
            routeIdx = routeIdx + 1
        end
    end


    return myGraph(Ports, Routes, portSets, incomingArcs, outgoingArcs, start, finish), seaMargin, rr
end

#=
Sorts routes so that they are in correct order. 
Input:
    routes: Routes that are in the solution
    start_node: Given origin node
    end_node: Given finish node
Output:
    routes in correct order going from start_node to end_node
=#
function sort_legs(routes, start_node, end_node)
    # Create a dictionary to map origins to legs
    route_map = Dict(route.or.nodeIndex => route for route in routes)

    # Initialize the sorted route list
    sorted_route = []  # Assuming Leg is the struct representing a leg

    # Start from the given origin
    current_node = start_node

    # Follow the sequence of legs until the end node is reached
    while current_node != end_node
        if haskey(route_map, current_node)
            route = route_map[current_node]
            push!(sorted_route, route)  # Add leg to the ordered route
            current_node = route.dest.nodeIndex  # Move to the next node
        end
    end

    return sorted_route
end

#=
Optimization model
Input:
    graph: Graph with routes and ports
    sm: Dict with sea margin INFO
    rr: routing restrictions associated with routingID
=#

function loadPrices(ports,n)
    df = CSV.read("C:/Users/vikto/Documents/DTU/Thesis/priceHistory/1yearPricesBiggerv2.csv", DataFrame)  # Read CSV into a DataFrame
    df.grade = Symbol.(df.grade)
    df.port = Symbol.(df.port)
    df.date = Date.(df.date, "dd/mm/yyyy")
    num_days = length(unique(df.date))  # Number of unique days in the dataset
    # Filter only rows where the date is after January 2024
    sort!(df, :date)
    fuel = [:MGO,:VLSFO]
    #indices = randperm(365)[1:3]
    #println(df)
    scenarios = []
    for f in fuel
        prices = []
        for i in ports
            #println(f, i.UNLOCODE)
            prices = df[(df.grade .== f) .& (df.port .== i.UNLOCODE), :price]
            if isempty(prices)
                prices = zeros(num_days)  # Return n zeros if no data
            end
            #print(i.UNLOCODE, ": ", prices[1], " *** ")

            #randomPrices = [rand(prices) for _ in 1:n]
            push!(scenarios, prices)
        end
    end

    return scenarios
end


graph, sm, rr = createInstance()

scenarios = loadPrices(graph.nodes, 31)
scenariosFixed = scenarios #[i in [7,14] ? fill(mean(scenarios[i]), length(scenarios[i])) : scenarios[i] for i in eachindex(scenarios)]
for i in 1:size(graph.nodes,1)
    scenariosFixed[i] .= scenariosFixed[i] .+ 256
end
prices = [mean(subarray) for subarray in scenarios]

Ports = graph.nodes
P = graph.nodes[1:6]
Por = graph.nodes[7]
Routes = graph.arcs
bunkerPorts = graph.portSets[1]
cargoPorts = graph.portSets[2]
bothPorts = graph.portSets[3]
inRoute = graph.incomingArcs
outRoute = graph.outgoingArcs
start = graph.start
finish = graph.finish
seaMargin = 1.1
routingRestriction = rr

Fuel = [:MGO, :VLSFO]

nPort = size(Ports,1)
nRoute = size(Routes,1)
nFuel = 2

delta = 10 #step size


#for i in 1:nPort
    #   prices[i] += 256
#end
#
#Whether fuel is available
a = ones(nPort, nFuel)
#=
a[34,1]=0
a[34,2]=0
a[35,1]=0
a[35,2]=0
a[36,1]=0
a[32,2]=0
=#

speed = 14*24 #nm/day
startFuel = [200.0, 500.0]
finishFuel = [300.0, 500.0]
maxFuel = [425,1700]
minFuel = [50,100]
consumption = 27 #mts/day
loadline = Dict(:Summer => 74000, :Winter => 70300, :Tropical => 77700)
fullDraftMeters = Dict(:Summer => 13.7, :Winter => 13.015, :Tropical => 14.385) # Only for visualization
fullDraft = [13.7, 13.015, 14.385]
tonPerCm = [(loadline[:Summer]-loadline[:Winter])/(fullDraft[1]-fullDraft[2])/100]
freightRate = 50
hireCost = 18000
#
fixedRoutingCost = Dict(:Skaw => 5000, :Kiel_Canal => 25000, :Other => 0, :Magellan => 20000, Symbol("Cape of Good Hope") => 0, :Panama => 30000)
M1 = 100000
M2 = 100000

alpha_down = 0.0

model = Model(Gurobi.Optimizer)
set_silent(model)

# Decision variables
@variable(model, x[1:nRoute], Bin)              # Route selection
@variable(model, v[1:nPort], Bin)               # Bunker visited
@variable(model, z >= 0)                        # Cargo transported
#@variable(model, o[1:nFuel] >= 0)

@variable(model, alpha)

# Objective
@objective(model, Max, - sum(x[l]*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute)                         #Routing costs
                       - sum(v[i]*Ports[i].agency_fee for i = 1:nPort)                                     #Agency fee
                       + freightRate * z                                                                  #Freight revenue
                       + alpha)

#Constraints
@constraint(model, sum(x[l] for l in outRoute[start]) == 1)
@constraint(model, sum(x[l] for l in inRoute[finish]) == 1)
@constraint(model, [i in bunkerPorts], sum(x[l] for l in outRoute[Ports[i]]) == sum(x[l] for l in inRoute[Ports[i]]))
@constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in outRoute[Ports[i]]))
@constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in inRoute[Ports[i]]))
@constraint(model, [i in 1:nPort], v[i] <= sum(x[l] for l in outRoute[Ports[i]]) + sum(x[l] for l in inRoute[Ports[i]]))
@constraint(model, alpha <= alpha_down)

@constraint(model, z <= 77700)
#@constraint(model, z == 51970.83068300323)
#@constraint(model, [g in [w for w in eachindex(Routes) if Routes[w].or.nodeIndex == start.nodeIndex && Routes[w].dest.nodeIndex == finish.nodeIndex]], sum(x[l] * sum(Routes[l].distance) for l in 1:nRoute) <= 1.5  * sum(Routes[g].distance))
@constraint(model, sum(x[l] for l in 1:nRoute)<=2 )

model


subproblem_model = Model(Gurobi.Optimizer)
set_silent(subproblem_model)

# Define subproblem-local variables (renamed to avoid conflicts)
@variable(subproblem_model, z_penalty_sub >= 0)
@variable(subproblem_model, x_sub[l in 1:nRoute])
@variable(subproblem_model, v_sub[i in 1:nPort])
@variable(subproblem_model, z_sub >= 0)
@variable(subproblem_model, y_sub[1:nPort, 1:nFuel] >= 0)
@variable(subproblem_model, b_sub[1:nPort, 1:nFuel] >= 0)
@variable(subproblem_model, t_sub >= 0)

# Bunker start/end
@constraint(subproblem_model, [f in 1:nFuel], b_sub[start.nodeIndex, f] == startFuel[f] + y_sub[start.nodeIndex, f])
@constraint(subproblem_model, [f in 1:nFuel], b_sub[finish.nodeIndex, f] >= finishFuel[f])

# Bunker balance
@constraint(subproblem_model, [f in 1:nFuel, l in 1:nRoute],
    b_sub[Routes[l].dest.nodeIndex, f] +
    consumption / speed * sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) -
    y_sub[Routes[l].dest.nodeIndex, f] -
    b_sub[Routes[l].or.nodeIndex, f] <= M1 * (1 - x_sub[l])
)
@constraint(subproblem_model, [f in 1:nFuel, l in 1:nRoute],
    b_sub[Routes[l].dest.nodeIndex, f] +
    consumption / speed * sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) -
    y_sub[Routes[l].dest.nodeIndex, f] -
    b_sub[Routes[l].or.nodeIndex, f] >= -M1 * (1 - x_sub[l])
)

# Bunker capacity
@constraint(subproblem_model, [f in 1:nFuel, i in 1:nPort], b_sub[i, f] <= v_sub[i] * maxFuel[f])
@constraint(subproblem_model, [f in 1:nFuel, i in 1:nPort], minFuel[f] + y_sub[i, f] - M2 * (1 - v_sub[i]) <= b_sub[i, f])

# Bunker only if fuel available
@constraint(subproblem_model, [f in 1:nFuel, i in 1:nPort], y_sub[i, f] <= M1 * a[i, f])

# Bunker min amount
@constraint(subproblem_model, [i in 1:nPort], Ports[i].min_bunker - sum(y_sub[i, f] for f in 1:nFuel) <= M1 * (1 - v_sub[i]))

# Time variable
@constraint(subproblem_model,
    t_sub ==
    sum(x_sub[l] * sum(Routes[l].distance) / speed for l in 1:nRoute) * seaMargin +
    sum(sum(y_sub[i, f] for f in 1:nFuel) * Ports[i].time_quantity for i in 1:nPort)
)

# Loadline ports weight
@constraint(subproblem_model, [i in 1:nPort],
    fullDraft[1] - (loadline[:Summer] - z_sub - sum(b_sub[i, f] for f in 1:nFuel)) / (tonPerCm[1] * 100)
    <= Ports[i].max_draft + M1 * (1 - v_sub[i])
)

# Loadline route draft
@constraint(subproblem_model, [l in 1:nRoute],
    sum(b_sub[Routes[l].or.nodeIndex, f] for f in 1:nFuel) + z_sub <= loadline[Routes[l].loadline[1]]
)
@constraint(subproblem_model, [l in 1:nRoute, k in 1:(Routes[l].nLegs - 1)],
    sum(b_sub[Routes[l].or.nodeIndex, f] for f in 1:nFuel) + z_sub -
    consumption / speed * sum(Routes[l].distance[d] for d = 1:k)
    <= loadline[Routes[l].loadline[k + 1]] + M1 * (1 - x_sub[l])
)

# Loadline routeID weight
@constraint(subproblem_model, [l in 1:nRoute],
    sum(b_sub[Routes[l].or.nodeIndex, f] for f in 1:nFuel) + z_sub -
    consumption / speed * Routes[l].distToID <= routingRestriction[Routes[l].ID][1] + M1 * (1 - x_sub[l])
)

# Loadline routeID draft
@constraint(subproblem_model, [l in 1:nRoute],
    fullDraft[1] - (loadline[:Summer] - z_sub - sum(b_sub[Routes[l].or.nodeIndex, f] for f in 1:nFuel) +
    consumption / speed * Routes[l].distToID) / (tonPerCm[1] * 100)
    <= routingRestriction[Routes[l].ID][2] + M1 * (1 - x_sub[l])
)

#= Objective
@objective(subproblem_model, Max,
    - M1 * z_penalty_sub
    - t_sub * hireCost
    - sum(sum(y_sub[i, f] * scenariosFixed[i + (nPort * (f - 1))][scenario_i] for f in 1:nFuel) for i in 1:nPort)
    + sum((b_sub[finish.nodeIndex, f] - finishFuel[f]) * scenariosFixed[finish.nodeIndex + (nPort * (f - 1))][scenario_i] for f in 1:nFuel)
)=#

# Return subproblem_model to caller
subproblem_model

function solve_subproblem!(scenario_i, x_bar, v_bar, z_bar)
    # Update fixed variables instead of creating new ones
    if @isdefined z_constraint_sub
        delete(subproblem_model, z_constraint_sub)
        z_constraint_sub = nothing
    end

    fix.(x_sub, x_bar; force = true)
    fix.(v_sub, v_bar; force = true)

    # Add updated constraint and store its reference
    global z_constraint_sub = @constraint(subproblem_model, z_sub == z_bar - z_penalty_sub)

    global subproblem_objective = @objective(subproblem_model, Max,
        - M1 * z_penalty_sub
        - t_sub * hireCost
        - sum(sum(y_sub[i, f] * scenariosFixed[i + (nPort * (f - 1))][scenario_i] for f in 1:nFuel) for i in 1:nPort)
        #+ sum((b_sub[finish.nodeIndex, f] - finishFuel[f]) * scenariosFixed[finish.nodeIndex + (nPort * (f - 1))][scenario_i] for f in 1:nFuel)
    )


    # Reoptimize the model
    optimize!(subproblem_model)

    if termination_status(subproblem_model) != MOI.OPTIMAL
        return (is_feasible = false, reason = :x)
    elseif value(z_penalty_sub) > 1e-6
        return (is_feasible = false, reason = :z, z = value(z_sub))
    else
        return (
            is_feasible = true,
            obj = objective_value(subproblem_model),
            y = value.(y_sub),
            b = value.(b_sub),
            t = value(t_sub),
            π_x = reduced_cost.(x_sub),
            π_v = reduced_cost.(v_sub),
            π_z = reduced_cost(z_sub)
        )
    end
end



function old_solve_subproblem(scenario, x_bar, v_bar, z_bar#=, o_bar=#)
    model = Model(Gurobi.Optimizer)
    set_silent(model)
    @variable(model, z_penalty >= 0)


    #Fixed var 
    @variable(model, x[l in 1:nRoute])# == x_bar[l])              # Route selection
    @variable(model, v[i in 1:nPort])# == v_bar[i])               # Bunker visited
    @variable(model, z >= 0)# == z_bar)                        # Cargo transported
    #@variable(model, o[f in 1:nFuel] )# == o_bar[f])                    # Fix origin
    #2nd stage var
    @variable(model, y[1:nPort, 1:nFuel] >= 0)      # Bunker purchase
    @variable(model, b[1:nPort, 1:nFuel] >= 0)      # Bunker level
    @variable(model, t >= 0)                        # Trip time
    #@variable(model, mu[1:nPort, 1:nFuel] >= 0)    # Step variable
    #constr
    fix.(x, x_bar)
    fix.(v, v_bar)
    @constraint(model, z == z_bar - z_penalty)
    #fix.(o, o_bar)
    #Bunker start/end
    @constraint(model, [f in 1:nFuel], b[start.nodeIndex, f] == startFuel[f] + y[start.nodeIndex,f])
    @constraint(model, [f in 1:nFuel], b[finish.nodeIndex, f] >= finishFuel[f])
    #Bunker steps
    #@constraint(model, [f in 1:nFuel, i in 1:nPort], y[i,f] <= delta * mu[i,f] + M1 * (1 - v[i]))
    #@constraint(model, [f in 1:nFuel, i in 1:nPort], y[i,f] >= delta * mu[i,f] - M1 * (1 - v[i]))
    #Bunker balance
    @constraint(model, [f in 1:nFuel, l in 1:nRoute], b[Routes[l].dest.nodeIndex, f] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f] - b[Routes[l].or.nodeIndex, f] <= M1 * (1 - x[l]))
    @constraint(model, [f in 1:nFuel, l in 1:nRoute], b[Routes[l].dest.nodeIndex, f] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f] - b[Routes[l].or.nodeIndex, f] >= -M1 * (1 - x[l]))    
    #Bunker capacity
    @constraint(model, [f in 1:nFuel, i in 1:nPort], b[i,f] <= v[i] * maxFuel[f])
    @constraint(model, [f in 1:nFuel, i in 1:nPort],  minFuel[f] + y[i,f] - M2 * (1 - v[i]) <= b[i,f]) 
    #Bunker only if fuel available
    @constraint(model, [f in 1:nFuel, i in 1:nPort], y[i,f] <= M1 * a[i,f])
    #Bunker min amount
    @constraint(model, [f in 1:nFuel, i in 1:nPort], Ports[i].min_bunker - sum(y[i,f] for f in 1:nFuel) <= M1 * (1 - v[i]))
    #Fix y_origin
    #@constraint(model, [f in 1:nFuel], y[start.nodeIndex, f] == o[f])
    # Time variable
    @constraint(model, t == sum(x[l] * sum(Routes[l].distance) / speed for l in 1:nRoute) * seaMargin + sum(sum(y[i,f] for f in 1:nFuel) * Ports[i].time_quantity for i in 1:nPort) )
    # Loadline ports weight
    @constraint(model, [i in 1:nPort], fullDraft[1] - (loadline[:Summer] - z - sum(b[i,f] for f in 1:nFuel)) / (tonPerCm[1] * 100) <= Ports[i].max_draft + M1 * (1 - v[i]))
    #Loadline routes draft
    @constraint(model, [l in 1:nRoute], sum(b[Routes[l].or.nodeIndex,f] for f in 1:nFuel) + z <= loadline[Routes[l].loadline[1]])
    @constraint(model, [l in 1:nRoute, k in 1:(Routes[l].nLegs - 1)], sum(b[Routes[l].or.nodeIndex,f] for f in 1:nFuel) + z - consumption/speed*sum(Routes[l].distance[d] for d = 1:k) <= loadline[Routes[l].loadline[k+1]] + M1 * (1 - x[l]))
    #Loadline routeID weight
    @constraint(model, [l in 1:nRoute], sum(b[Routes[l].or.nodeIndex,f] for f in 1:nFuel) + z - consumption / speed * Routes[l].distToID <= routingRestriction[Routes[l].ID][1] + M1 * (1 - x[l]) )
    #Loadline routeID draft
    @constraint(model, [l in 1:nRoute], fullDraft[1] - (loadline[:Summer] - z - sum(b[Routes[l].or.nodeIndex,f] for f in 1:nFuel) + consumption / speed * Routes[l].distToID) / (tonPerCm[1] * 100) <= routingRestriction[Routes[l].ID][2] + M1 * (1 - x[l]))

    @objective(model, Max, - M1*z_penalty
                            - t * hireCost
                            - sum( sum( y[i,f] * scenariosFixed[i+(nPort*(f-1))][scenario] for f = 1:nFuel) for i = 1:nPort) #Bunker costs
                            + sum( (b[finish.nodeIndex,f] - finishFuel[f]) * scenariosFixed[finish.nodeIndex+(nPort*(f-1))][scenario] for f = 1:nFuel))
    optimize!(model)

    if termination_status(model) != MOI.OPTIMAL
        #println("No good route")
        return (is_feasible = false, reason = :x)
    elseif value(z_penalty) > 1e-6
        #println("pen: ", value(z_penalty))
        return (is_feasible = false, reason = :z, z = value(z))
    else
        #println("z value: ", value(z))
        return (is_feasible = true, obj = objective_value(model), #=z = value(z),=# y = value.(y), b = value.(b), t = value(t), π_x = reduced_cost.(x), π_v = reduced_cost.(v), π_z = reduced_cost(z)#=, π_o = reduced_cost.(o)=#)
    end
end


function print_iteration(k, args...)
    f(x) = Printf.@sprintf("%12.4e", x)
    println(lpad(k, 9), " ", join(f.(args), " "))
    return
end


MAXIMUM_ITERATIONS = 10000
ABSOLUTE_OPTIMALITY_GAP = 1e-6
num_subproblems = 365
println("Iteration  Lower Bound  Upper Bound          Gap")

elapsed_time = @elapsed begin
    for k in 1:MAXIMUM_ITERATIONS
        optimize!(model)
        status = termination_status(model)
        if status != MOI.OPTIMAL && status != MOI.LOCALLY_SOLVED
            error("Master problem not solved optimally or feasibly at iteration $k. Status: $status")
        end
        upper_bound = objective_value(model)
        x_k = value.(x)
        v_k = value.(v)
        z_k = value(z)
        #o_k = value.(o)
        #println(z_k,v_k)

        total_obj = 0.0
        total_π_x = zeros(length(x))
        total_π_v = zeros(length(v))
        total_π_z = 0.0
        no_infeasible = true
        reas = :none

        z_feasible = 1e6

        for i in 1:num_subproblems
            ret_i = solve_subproblem!(i, x_k, v_k, z_k)
            if !ret_i.is_feasible
                if ret_i.reason ==:x
                    reas = :x
                    no_infeasible = false
                    break
                else
                    reas = :z
                    z_feasible = min(z_feasible, ret_i.z)
                    no_infeasible = false
                    break
                end
            else
                total_obj += (ret_i.obj / num_subproblems)
                total_π_x .+= (ret_i.π_x / num_subproblems)
                total_π_v .+= (ret_i.π_v / num_subproblems)
                total_π_z += (ret_i.π_z / num_subproblems)
            end
        end
        if no_infeasible
            lower_bound = (objective_value(model) - value(alpha)) + total_obj #ret.obj
            gap = abs(upper_bound - lower_bound) / abs(upper_bound)
            print_iteration(k, lower_bound, upper_bound, gap)
            if gap < ABSOLUTE_OPTIMALITY_GAP
                println("Terminating with the optimal solution")
                break
            end
            cut = @constraint(model, alpha <= total_obj + sum(total_π_x .* (x .- x_k)) + sum(total_π_v .* (v .- v_k)) + total_π_z * (z - z_k))
            #@info "Adding the Benders cut $(cut)"
        else
            println("Iteration ", k)
            if reas == :x
                cut = @constraint(model, sum(x[i] for i in 1:nRoute if x_k[i] == 0) + sum(1 - x[i] for i in 1:nRoute if x_k[i] == 1) >= 1)
                #cut = @constraint(model, sum(v[i] for i in 1:nPort if v_k[i] == 0) + sum(1 - v[i] for i in 1:nPort if v_k[i] == 1) >= 1)
                @info "Adding x cut"# $(cut)"

            else
                cut = @constraint(model, z <= z_feasible - 1e-6  + M1 * (
                    sum(x .* (1 .- x_k) + (1 .- x) .* x_k)
                        )   
                    )
                @info "Adding z cut"# $(cut)"
            end
        end
        #ret = solve_subproblem(x_k, v_k, z_k)#, o_k)

        #cut = @constraint(model, alpha >= ret.obj + sum(ret.π_x .* (x .- x_k)) + sum(ret.π_v .* (v .- v_k)) + sum(ret.π_z * (z - z_k)))# + sum(ret.π_o .* (o .- o_k)))
        #@info "Adding the cut $(cut)"
    end
end

println("Elapsed time: $(round(elapsed_time, digits=2)) seconds")


optimize!(model)

#assert_is_solved_and_feasible(model)
println("final values")
x_optimal = value.(x)
v_optimal = value.(v)
z_optimal = value(z)
#ret_optimal = solve_subproblem!(1, x_optimal, v_optimal, z_optimal)
#y_optimal = ret_optimal.y
#b_optimal = ret_optimal.b
#t_optimal = ret_optimal.t
obj_optimal = objective_value(model)

#println("Optimal x: ", x_optimal)
println("Optimal v: ", v_optimal)
println("Optimal z: ", z_optimal)
#println("Optimal y: ", y_optimal)
#println("Optimal b: ", b_optimal)
#println("Optimal t: ", t_optimal)
println("Optimal objective value: ", obj_optimal)
println("1st ", obj_optimal - value(alpha))
println("2nd ", value(alpha)) #Bunker costs
#println("Running with $(Threads.nthreads()) threads")
