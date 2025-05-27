#Viktor Andri Henriksen

module implement

using Random, Distributions
using JuMP
using Gurobi
using DataFrames
using XLSX
using Plots
using CSV
using Dates
using Clustering
using Distances
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
function optimizeRoute(graph::myGraph, sm, rr, prices)
    model = Model(Gurobi.Optimizer)
    #set_silent(model)

    Ports = graph.nodes
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

    delta = 50 #step size

    nPort = size(Ports,1)
    nRoute = size(Routes,1)
    nFuel = 2

    a = ones(nPort, nFuel)
    #=For DataBigger:
    a[34,1]=0
    a[34,2]=0
    a[35,1]=0
    a[35,2]=0
    a[36,1]=0
    a[32,2]=0
 =#

    #prices = [ismissing(x) || isnan(x) ? 0.0 : x for x in prices]
    i = 1
    for p in graph.nodes
        print(i, " ", p.name, ".  ")
        i = i+1
    end

    #prices = prices.*(0.5 .+ 1 .* rand(1, 7))
    #println(prices)

 #=needs to be implemented to Vessel struct
    speed = 14*24 #nm/day
    startFuel = [200.0, 200.0]
    finishFuel = [200.0, 200.0]
    maxFuel = [225,325]
    minFuel = [50,100]
    consumption = 15 #mts/day
    loadline = Dict(:Summer => 28200, :Winter => 26790, :Tropical => 29610)
    fullDraftMeters = Dict(:Summer => 9.7, :Winter => 9.215, :Tropical => 10.185) # Only for visualization
    fullDraft = [9.7, 9.215, 10.185]
    tonPerCm = [(loadline[:Summer]-loadline[:Winter])/(fullDraft[1]-fullDraft[2])/100]
    freightRate = 55
    hireCost = 9000
 =#

    speed = 14*24 #nm/day
    startFuel = [200.0, 500.0]
    finishFuel = [300.0, 500.0]
    #startFuel = [50.0, 100.0]
    #finishFuel = [150.0, 100.0]
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

    # Decision variables
    @variable(model, x[1:nRoute], Bin)              # Route selection
    @variable(model, v[1:nPort], Bin)               # Bunker visited
    @variable(model, y[1:nPort, 1:nFuel] >= 0)      # Bunker purchase
    @variable(model, b[1:nPort, 1:nFuel] >= 0)      # Bunker level
    @variable(model, z >= 0)                        # Cargo transported
    @variable(model, t >= 0)                        # Trip time
    #@variable(model, mu[1:nPort, 1:nFuel] >= 0, Int)    # Step variable

    # Objective: Maximize profit
    @objective(model, Max, - t * hireCost
                        - sum( x[l] * fixedRoutingCost[Routes[l].ID] for l = 1:nRoute)                 #Routing costs
                        - sum( v[i] * Ports[i].agency_fee for i = 1:nPort)                             #Agency fee
                        - sum( sum( y[i,f] * prices[i+(nPort*(f-1))] for f = 1:nFuel) for i = 1:nPort) #Bunker costs
                        + freightRate * z                                                         #Freight revenue
                        + sum( (b[finish.nodeIndex,f] - finishFuel[f]) * prices[finish.nodeIndex+(nPort*(f-1))] for f = 1:nFuel))
    # Network Flow 
    @constraint(model, sum(x[l] for l in outRoute[start]) == 1)
    @constraint(model, sum(x[l] for l in inRoute[finish]) == 1)
    @constraint(model, [i in bunkerPorts], sum(x[l] for l in outRoute[Ports[i]]) == sum(x[l] for l in inRoute[Ports[i]]))


    #Auxiliary if port visited
    @constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in outRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in inRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort], v[i] <= sum(x[l] for l in outRoute[Ports[i]]) + sum(x[l] for l in inRoute[Ports[i]]))


    #Bunker start/end
    @constraint(model, [f in 1:nFuel], b[start.nodeIndex, f] == startFuel[f] + y[start.nodeIndex,f])
    @constraint(model, [f in 1:nFuel], b[finish.nodeIndex, f] >= finishFuel[f])
    #Bunker steps
    #@constraint(model, [f in 1:nFuel, i in 1:nPort], y[i,f] == delta * mu[i,f])
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


    #Sensitivity analysis stuff
    #@constraint(model, sum(v[i] for i = 1:nPort)==5)
    #@constraint(model, z==0)

    #@constraint(model, sum(y[7,i] for i = 1:2) == 0)
    optimize!(model)

    println("Termination status: ", termination_status(model))
    println("Primal status: ", primal_status(model))
 ######################################################################################################
 #Print information and create plots
 ######################################################################################################

 #Initialize arrays for plots
    path = []
    lvlMGO = [startFuel[1]]
    lvlVLSFO = [startFuel[2]]
    stops = [start.name]
    distances = [0]
    distancesPorts = [0]
    draft = []
    draftPorts = []
    draftActual = [fullDraft[1] - (loadline[:Summer] - value(z) - sum(value(b[start.nodeIndex,f]) for f in 1:nFuel)+sum(value(y[start.nodeIndex,f]) for f in 1:nFuel)) / (tonPerCm[1] * 100), fullDraft[1] - (loadline[:Summer] - value(z) - sum(value(b[start.nodeIndex,f]) for f in 1:nFuel)) / (tonPerCm[1] * 100)]
    portLabels = [start.name]
    agFee = 0.0
    fuelCosts = 0.0

    println("******************************************************************************")
    println("ROUTE INFO")
 #Printing Route information
    for l = 1:nRoute
        if isapprox(value(x[l]), 1)
            push!(path,Routes[l])
        end
    end

    path = sort_legs(path, start.nodeIndex, finish.nodeIndex)

    for l = eachindex(path)
        print("From ", path[l].or.name, " to ", path[l].dest.name, " via ", path[l].ID, ". ", "Routing cost ", fixedRoutingCost[path[l].ID], ". ")
        print(" Consumption MGO: ", round(consumption/speed*sum(path[l].distance[d] for d = 1:path[l].nLegs if path[l].fuel_type[d] == :MGO; init = 0), digits = 2))
        print("legnr:", l, " Consumption VLSFO: ", round(consumption/speed*sum(path[l].distance[d] for d = 1:path[l].nLegs if path[l].fuel_type[d] == :VLSFO; init = 0), digits = 2))
        println()
        append!(distances, last(distances) .+ [y for x in cumsum(path[l].distance) for y in (x,x)])
        append!(draft, [y for x in [fullDraftMeters[season] for season in path[l].loadline] for y in (x,x)])
        push!(distancesPorts, last(distancesPorts) + sum(path[l].distance))
        push!(draftPorts, path[l].or.max_draft)
        push!(portLabels, path[l].dest.name)
        push!(draftActual, fullDraft[1] - (loadline[:Summer] - value(z) - sum(value(b[path[l].dest.nodeIndex,f]) for f in 1:nFuel)+sum(value(y[path[l].dest.nodeIndex,f]) for f in 1:nFuel)) / (tonPerCm[1] * 100))
        push!(draftActual, fullDraft[1] - (loadline[:Summer] - value(z) - sum(value(b[path[l].dest.nodeIndex,f]) for f in 1:nFuel)) / (tonPerCm[1] * 100))
    end
    pop!(distances)
    push!(draftPorts, last(path).dest.max_draft)

    println()

 #First iteration of printing ports visited
    print("fuel costs are not udated corretly in here")
    print("Port ", path[1].or.name, " visited.")
    print(" Agency fee is ", path[1].or.agency_fee)
    for j = 1:nFuel
        print(" -- Fuel level type $j: ", round(value(b[path[1].or.nodeIndex,j]), digits=2))
        if value(y[path[1].or.nodeIndex,j]) > 0
            print(" -- Fuel bunkered type $j: ", round(value(y[path[1].or.nodeIndex, j]), digits=2), " cost ", round(value(y[path[1].or.nodeIndex,j])*path[1].or.bunker_price[j], digits=2))
            fuelCosts = fuelCosts + value(y[path[1].or.nodeIndex,j])*path[1].or.bunker_price[j]
        end
    end
    if value(y[path[1].or.nodeIndex,1]) > 0 || value(y[path[1].or.nodeIndex,2]) > 0 
        push!(stops, path[1].or.name)
        push!(lvlVLSFO, value(b[path[1].or.nodeIndex,2]))
        push!(lvlMGO, value(b[path[1].or.nodeIndex,1]))
    end
    println()
 #Following iterations of printing ports visited
    for i = eachindex(path)
        print("Port ", path[i].dest.name, " visited.")
        print(" Agency fee is ", path[i].dest.agency_fee)
        agFee = agFee + path[i].dest.agency_fee
        push!(lvlMGO, value(b[path[i].dest.nodeIndex,1])  - value(y[path[i].dest.nodeIndex,1]))
        push!(lvlVLSFO, value(b[path[i].dest.nodeIndex,2])  - value(y[path[i].dest.nodeIndex,2]))
        push!(stops, path[i].dest.name)
        for j = 1:nFuel
            print(" -- Fuel level type $j: ", round(value(b[path[i].dest.nodeIndex,j]), digits=2))
            if value(y[path[i].dest.nodeIndex,j]) > 0
                print(" -- Fuel bunkered type $j: ", round(value(y[path[i].dest.nodeIndex, j]), digits=2), " cost ", round(value(y[path[i].dest.nodeIndex,j])*path[i].dest.bunker_price[j], digits=2))
                fuelCosts = fuelCosts + value(y[path[i].dest.nodeIndex,j])*path[i].dest.bunker_price[j]
            end
        end
        if value(y[path[i].dest.nodeIndex,1]) > 0 || value(y[path[i].dest.nodeIndex,2]) > 0 
            push!(stops, path[i].dest.name)
            push!(lvlMGO, value(b[path[i].dest.nodeIndex,1]))
            push!(lvlVLSFO, value(b[path[i].dest.nodeIndex,2]))
        end
        println()
    end
 #Printing additional info
    println()
    println("Total time is ", round(value(t), digits=1), " days.")
    println("Cargo carried is ", round(value(z), digits=1), " tonnes.")
    println("Hire cost is ", round(value(t) * hireCost, digits=2) )
    println("Freight revenue is ", round(freightRate * value(z), digits=2))
    println("Agency fees add up to ", agFee)
    println("Bunker fuel costs add up to ", round(sum(sum(value(y[i,f])*prices[i+(nPort*(f-1))] for f = 1:nFuel) for i = 1:nPort), digits = 2))
    println("Fixed routing costs add up to ", round(sum(value(x[l])*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute), digits = 2))
    println("Profit is ", round(objective_value(model), digits=1))
    println("******************************************************************************")
    println("Fuel burnt: ", sum(sum(value(y[i,f]) for f = 1:nFuel) for i = 1:nPort))
    #println("y ", value.(y))
    #println(value.(mu))
 #Making plots
    stops_str = [string(s) for s in stops]
    labels_str = [string(s) for s in portLabels]


    plot1=plot(stops_str, lvlMGO, label="Bunker load, MGO", linewidth=2, marker=:circle, color=:blue)
    plot!(stops_str, lvlVLSFO, label="Bunker load, VLSFO", linewidth=2, marker=:diamond, color=:red)
    xlabel!("Route")
    ylabel!("Load [mt]")
    #title!("Bunker load on route")
    display(plot1)
    #savefig(plot1, "fuel.png")

    plot2=plot(distances, draft, label="Load line draft restriction", legend=:topright, ylimits=(minimum(draftActual)-0.5,maximum(draft)+0.5), linewidth=2, color=:blue)
    scatter!(distancesPorts, draftPorts, label="Port draft restriction", legend=:topright, marker=:star, color=:yellow)
    xticks!(distancesPorts,labels_str)
    plot!([y for x in distancesPorts for y in (x,x)], draftActual, label="Ship's draft", legend=:topright, linewidth=2, color=:red)
    yflip!(true)
    xlabel!("Route")
    ylabel!("Meters below sea level")
    #title!("Draft levels")
    display(plot2)
    #savefig(plot2, "draft.png")

    println("x: ",value.(x))
    println("v: ",value.(v))
    println("z: ",value(z))
    println("y: ",value.(y))
    println("b: ",value.(b))
    println("t: ",value(t))
    println("obj: ",objective_value(model))
    st = (- sum( value(x[l]) * fixedRoutingCost[Routes[l].ID] for l = 1:nRoute)                 #Routing costs
        - sum( value(v[i]) * Ports[i].agency_fee for i = 1:nPort)                             #Agency fee
        + freightRate * value(z)   )
    nd = (- value(t) * hireCost
    - sum( sum( value(y[i,f]) * prices[i+(nPort*(f-1))] for f = 1:nFuel) for i = 1:nPort) #Bunker costs
    + sum( (value(b[finish.nodeIndex,f]) - finishFuel[f]) * prices[finish.nodeIndex+(nPort*(f-1))] for f = 1:nFuel))
    println("1st:", st)
    println("2nd:", nd)


    return model, round(objective_value(model), digits=1), value(z), value.(v), value.(y), value(t)
end


function makeScenario(ports,n)
    scenarios = []
    for _ = 1:n
        MGO = Vector{Int64}()
        VLSFO = Vector{Int64}()
        for i in ports 
            p = i.bunker_price
            if p[1]==0
                push!(MGO, 0)
            else
                push!(MGO, rand(floor(Int, 0.7*p[1]):ceil(Int, 1.3*p[1])))
            end
            if p[2]==0
                push!(VLSFO, 0)
            else
                push!(VLSFO, rand(floor(Int, 0.7*p[2]):ceil(Int, 1.3*p[2])))
            end
        end
        push!(scenarios,[MGO,VLSFO])
    end
    return scenarios
end

#=
Loading prices from CSV file and creating scenarios from them
Input:
    ports: Array of ports that should be generated prices for
    n: number of scenarios to be generated
Output:
    scenarios: scenarios as an array of arrays with prices for each scenario. Assume m elements in ports. First m arrays in scenarios will be MGO, last m arrays will be VLSFO.
               [[MGOport1 scenarios], [MGOport2 scenarios], ..., [VLSFOport2scenarios], ...]
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

function optimizeRouteTwoStage(graph::myGraph, sm, rr, prices)
    model = Model(Gurobi.Optimizer)

    #+-10% of mean prices
    #prices = [mean(subarray) for subarray in prices]
    #println(prices)
    #prices = [[x * (rand() * 0.4 + 0.8) for _ in 1:1000] for x in prices]
    #println(prices)


    nScenarios = size(prices[1],1)
    #println("size ", nScenarios)


    Ports = graph.nodes
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

 #=needs to be implemented to Vessel struct
    speed = 14*24 #nm/day
    startFuel = [200.0, 200.0]
    finishFuel = [200.0, 200.0]
    maxFuel = [225,315]
    minFuel = [50,100]
    consumption = 15 #mts/day
    loadline = Dict(:Summer => 28200, :Winter => 26790, :Tropical => 29610)
    fullDraftMeters = Dict(:Summer => 9.7, :Winter => 9.215, :Tropical => 10.185) # Only for visualization
    fullDraft = [9.7, 9.215, 10.185]
    tonPerCm = [(loadline[:Summer]-loadline[:Winter])/(fullDraft[1]-fullDraft[2])/100]
    freightRate = 55
    hireCost = 18000
 =#

    speed = 14*24 #nm/day
    startFuel = [200.0, 500.0]
    finishFuel = [300.0, 500.0]
    #startFuel = [300.0, 1000.0]
    #finishFuel = [150.0, 100.0]
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

    println(Ports)

    # Decision variables
    @variable(model, x[1:nRoute], Bin)              # Route selection
    @variable(model, v[1:nPort], Bin)               # Bunker visited
    @variable(model, y[1:nPort, 1:nFuel, 1:nScenarios] >= 0)      # Bunker purchase
    @variable(model, b[1:nPort, 1:nFuel, 1:nScenarios] >= 0)      # Bunker level
    @variable(model, z >= 0)                        # Cargo transported
    @variable(model, t[1:nScenarios] >= 0)                        # Trip time
    #@variable(model, o[1:nFuel])
    #@variable(model, mu[1:nPort, 1:nFuel, 1:nScenarios] >= 0, Int)    # Step variable

    # Objective: Maximize profit
    @objective(model, Max, - sum(t[s] for s = 1:nScenarios) * hireCost * (1/nScenarios)
                        - sum(x[l]*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute)                         #Routing costs
                        - sum(v[i]*Ports[i].agency_fee for i = 1:nPort)                                     #Agency fee
                        - sum(sum(sum(y[i,f,s] * prices[i+(nPort*(f-1))][s] for f=1:nFuel) for i = 1:nPort) for s=1:nScenarios) * (1/nScenarios) #Bunker costs
                        + freightRate * z)                                                                  #Freight revenue
                        #+ sum(sum( (b[finish.nodeIndex,f,s] - finishFuel[f]) * prices[finish.nodeIndex+(nPort*(f-1))][s] for f = 1:nFuel) for s = 1:nScenarios) * (1/nScenarios)) 

    # Network Flow 
    @constraint(model, sum(x[l] for l in outRoute[start]) == 1)
    @constraint(model, sum(x[l] for l in inRoute[finish]) == 1)
    @constraint(model, [i in bunkerPorts], sum(x[l] for l in outRoute[Ports[i]]) == sum(x[l] for l in inRoute[Ports[i]]))


    #Auxiliary if port visited
    @constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in outRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort], v[i] >= sum(x[l] for l in inRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort], v[i] <= sum(x[l] for l in outRoute[Ports[i]]) + sum(x[l] for l in inRoute[Ports[i]]))


    #Bunker start/end
    @constraint(model, [f in 1:nFuel, s in 1:nScenarios], b[start.nodeIndex, f, s] == startFuel[f] + y[start.nodeIndex, f, s])
    @constraint(model, [f in 1:nFuel, s in 1:nScenarios], b[finish.nodeIndex, f, s] >= finishFuel[f])
    #Bunker steps
    #@constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], y[i,f,s] == delta * mu[i,f,s])
    #@constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], y[i,f,s] >= delta * mu[i,f,s] - M1 * (1 - v[i]))
    #Bunker only if available
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], y[i,f,s] <= M1 * a[i,f])
    #Bunker balance
    @constraint(model, [f in 1:nFuel, l in 1:nRoute, s in 1:nScenarios], b[Routes[l].dest.nodeIndex, f, s] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f, s] - b[Routes[l].or.nodeIndex, f, s] <= M1 * (1 - x[l]))
    @constraint(model, [f in 1:nFuel, l in 1:nRoute, s in 1:nScenarios], b[Routes[l].dest.nodeIndex, f, s] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f, s] - b[Routes[l].or.nodeIndex, f, s] >= -M1 * (1 - x[l]))
    #Bunker capacity
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], b[i,f,s] <= v[i] * maxFuel[f])
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios],  minFuel[f] + y[i,f,s] - M2 * (1 - v[i]) <= b[i,f,s]) 
    #Bunker min amount
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], Ports[i].min_bunker - sum(y[i,f,s] for f in 1:nFuel) <= M1 * (1 - v[i]))


    # Loadline ports weight
    @constraint(model, [i in 1:nPort, s in 1:nScenarios], fullDraft[1] - (loadline[:Summer] - z - sum(b[i,f,s] for f in 1:nFuel)) / (tonPerCm[1] * 100) <= Ports[i].max_draft + M1 * (1 - v[i]))
    #Loadline routes draft
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z <= loadline[Routes[l].loadline[1]])
    @constraint(model, [l in 1:nRoute, k in 1:(Routes[l].nLegs - 1), s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z - consumption/speed*sum(Routes[l].distance[d] for d = 1:k) <= loadline[Routes[l].loadline[k+1]] + M1 * (1 - x[l]))
    #Loadline routeID draft
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], fullDraft[1] - (loadline[:Summer] - z - sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + consumption / speed * Routes[l].distToID) / (tonPerCm[1] * 100) <= routingRestriction[Routes[l].ID][2] + M1 * (1 - x[l]))
    #Loadline routeID weight
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z - consumption / speed * Routes[l].distToID <= routingRestriction[Routes[l].ID][1] + M1 * (1 - x[l]) )
  
    
    # Time variable
    @constraint(model, [s in 1:nScenarios], t[s] == sum(x[l] * sum(Routes[l].distance) / speed for l in 1:nRoute) * seaMargin + sum(sum(y[i,f,s] for f in 1:nFuel) * Ports[i].time_quantity for i in 1:nPort) )

    #Fix 1st stage bunkering
    #@constraint(model, [f in 1:nFuel, s in 1:nScenarios], y[start.nodeIndex, f, s] == o[f])

    #Sensitivity analysis stuff
    #@constraint(model, sum(x[l] for l = 1:nRoute)<=2)
    @constraint(model, [g in [w for w in eachindex(Routes) if Routes[w].or.nodeIndex == start.nodeIndex && Routes[w].dest.nodeIndex == finish.nodeIndex]], sum(x[l] * sum(Routes[l].distance) for l in 1:nRoute) <= 1.5 * sum(Routes[g].distance))
    #@constraint(model, [f in 1:nFuel, s in 1:nScenarios], y[1,f,s] >= 0.1)
    #@constraint(model, z == 0)
    optimize!(model)

 ######################################################################################################
 #Print information and create plots
 ######################################################################################################

    path = []
    agFee = 0.0
    
    println("******************************************************************************")
    println("ROUTE INFO")
 #Printing Route information
    for l = 1:nRoute
        if isapprox(value(x[l]), 1)
            push!(path,Routes[l])
        end
    end

    path = sort_legs(path, start.nodeIndex, finish.nodeIndex)

    for l = eachindex(path)
        print("From ", path[l].or.name, " to ", path[l].dest.name, " via ", path[l].ID, ". ", "Routing cost ", fixedRoutingCost[path[l].ID], ". ")
        print(" Consumption MGO: ", round(consumption/speed*sum(path[l].distance[d] for d = 1:path[l].nLegs if path[l].fuel_type[d] == :MGO; init = 0), digits = 2))
        println(" Consumption VLSFO: ", round(consumption/speed*sum(path[l].distance[d] for d = 1:path[l].nLegs if path[l].fuel_type[d] == :VLSFO; init = 0), digits = 2))
    end

    println()

 #First iteration of printing ports visited
    print("Port ", path[1].or.name, " visited.")
    print(" Agency fee is ", path[1].or.agency_fee)

 #Following iterations of printing ports visited
    for i = eachindex(path)
        print("Port ", path[i].dest.name, " visited.")
        print(" Agency fee is ", path[i].dest.agency_fee)
        agFee = agFee + path[i].dest.agency_fee
    end
 #Printing additional info
    println()
    println("Avg time is ", round(sum(value(t[s]) for s = 1:nScenarios) * (1/nScenarios), digits=1), " days.")
    println("Cargo carried is ", round(value(z), digits=1), " tonnes.")
    println("Avg hire cost is ", round(sum(value(t[s]) for s = 1:nScenarios) * (1/nScenarios) * hireCost, digits=2) )
    println("Freight revenue is ", round(freightRate * value(z), digits=2))
    println("Agency fees add up to ", agFee)
    println("On avg bunker fuel costs add up to ", round(sum(sum(sum(value(y[i,f,s]) * prices[i+(nPort*(f-1))][s] for f=1:nFuel) for i = 1:nPort) for s=1:nScenarios) * (1/nScenarios), digits = 2))
    println("Fixed routing costs add up to ", round(sum(value(x[l])*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute), digits = 2))
    println("Profit is ", round(objective_value(model), digits=1))
    println("******************************************************************************")
    #println("Fuel burnt: ", sum(sum(value(y[i,f]) for f = 1:nFuel) for i = 1:nPort))

    println("1st stage: ", - sum(value(x[l])*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute) - sum(value(v[i])*Ports[i].agency_fee for i = 1:nPort)+ freightRate * value(z))
    println("2nd stage: ", - sum(value(t[s]) for s = 1:nScenarios) * hireCost * (1/nScenarios) - sum(sum(sum(value(y[i,f,s]) * prices[i+(nPort*(f-1))][s] for f=1:nFuel) for i = 1:nPort) for s=1:nScenarios) * (1/nScenarios)+ sum(sum( (value(b[finish.nodeIndex,f,s]) - finishFuel[f]) * prices[finish.nodeIndex+(nPort*(f-1))][s] for f = 1:nFuel) for s = 1:nScenarios) * (1/nScenarios))

    
    return model, round(objective_value(model), digits=1), value(z), value.(b), value.(y), value.(t), value.(v)
end

function optimizeRouteTwoStageTest(graph::myGraph, sm, rr, prices)
    model = Model(Gurobi.Optimizer)

    #prices = makeScenario(graph.nodes, scenarios)
    #nScenarios = scenarios

    nScenarios = size(prices[1],1)


    Ports = graph.nodes
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
    println(Ports)
    
    Fuel = [:MGO, :VLSFO]

    nPort = size(Ports,1)
    nRoute = size(Routes,1)
    nFuel = 2

    #Whether fuel is available
    a = ones(nPort, nFuel)

 #needs to be implemented to Vessel struct
    speed = 14*24 #nm/day
    startFuel = [200.0, 200.0]
    finishFuel = [200.0, 500.0]
    maxFuel = [225,900]
    minFuel = [50,100]
    consumption = 15 #mts/day
    loadline = Dict(:Summer => 28200, :Winter => 26790, :Tropical => 29610)
    fullDraftMeters = Dict(:Summer => 9.7, :Winter => 9.215, :Tropical => 10.185) # Only for visualization
    fullDraft = [9.7, 9.215, 10.185]
    tonPerCm = [(loadline[:Summer]-loadline[:Winter])/(fullDraft[1]-fullDraft[2])/100]
    freightRate = 55

    hireCost = 9000

    fixedRoutingCost = Dict(:Skaw => 5000, :Kiel_Canal => 25000, :Other => 0)
    M1 = 100000
    M2 = 100000

    # Decision variables
    #@variable(model, x[1:nRoute, 1:nScenarios], Bin)              # Route selection
    #@variable(model, v[1:nPort, 1:nScenarios], Bin)               # Bunker visited
    @variable(model, x[1:nRoute, 1:nScenarios], Bin)
    @variable(model, v[1:nPort, 1:nScenarios], Bin)
    @variable(model, y[1:nPort, 1:nFuel, 1:nScenarios] >= 0)      # Bunker purchase
    @variable(model, b[1:nPort, 1:nFuel, 1:nScenarios] >= 0)      # Bunker level
    @variable(model, z >= 0)                        # Cargo transported
    @variable(model, t[1:nScenarios] >= 0)                        # Trip time
    @variable(model, o[1:nFuel] >= 0)                        # Origin bunker purchase

    # Objective: Maximize profit
    @objective(model, Min, sum(t[s] for s = 1:nScenarios) * hireCost * (1/nScenarios)
                        + sum(sum(x[l,s]*fixedRoutingCost[Routes[l].ID] for l = 1:nRoute) for s=1:nScenarios) * (1/nScenarios)      #Routing costs
                        + sum(sum(v[i,s]*Ports[i].agency_fee for i = 1:nPort) for s=1:nScenarios) * (1/nScenarios)                #Agency fee
                        + sum(sum(sum(y[i,f,s] * prices[i+(nPort*(f-1))][s] for f=1:nFuel) for i = 1:nPort) for s=1:nScenarios) * (1/nScenarios) #Bunker costs
                        - freightRate * z)                                                                  #Freight revenue

    # Network Flow 
    @constraint(model, [s in 1:nScenarios], sum(x[l,s] for l in outRoute[start]) == 1)
    @constraint(model, [s in 1:nScenarios], sum(x[l,s] for l in inRoute[finish]) == 1)
    @constraint(model, [i in bunkerPorts, s in 1:nScenarios], sum(x[l,s] for l in outRoute[Ports[i]]) == sum(x[l,s] for l in inRoute[Ports[i]]))


    #Auxiliary if port visited
    @constraint(model, [i in 1:nPort, s in 1:nScenarios], v[i,s] >= sum(x[l,s] for l in outRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort, s in 1:nScenarios], v[i,s] >= sum(x[l,s] for l in inRoute[Ports[i]]))
    @constraint(model, [i in 1:nPort, s in 1:nScenarios], v[i,s] <= sum(x[l,s] for l in outRoute[Ports[i]]) + sum(x[l,s] for l in inRoute[Ports[i]]))


    #Bunker start/end
    @constraint(model, [f in 1:nFuel, s in 1:nScenarios], b[start.nodeIndex, f, s] == startFuel[f] + y[start.nodeIndex, f, s])
    @constraint(model, [f in 1:nFuel, s in 1:nScenarios], b[finish.nodeIndex, f, s] >= finishFuel[f])
    #Bunker only if available
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], y[i,f,s] <= M1 * a[i,f])
    #Bunker balance
    @constraint(model, [f in 1:nFuel, l in 1:nRoute, s in 1:nScenarios], b[Routes[l].dest.nodeIndex, f, s] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f, s] - b[Routes[l].or.nodeIndex, f, s] <= M1 * (1 - x[l,s]))
    @constraint(model, [f in 1:nFuel, l in 1:nRoute, s in 1:nScenarios], b[Routes[l].dest.nodeIndex, f, s] + consumption/speed*sum(Routes[l].distance[d] for d = 1:Routes[l].nLegs if Routes[l].fuel_type[d] == Fuel[f]) - y[Routes[l].dest.nodeIndex, f, s] - b[Routes[l].or.nodeIndex, f, s] >= -M1 * (1 - x[l,s]))
    #Bunker capacity
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios], b[i,f,s] <= v[i,s] * maxFuel[f])
    @constraint(model, [f in 1:nFuel, i in 1:nPort, s in 1:nScenarios],  minFuel[f] + y[i,f,s] - M2 * (1 - v[i,s]) <= b[i,f,s]) 
    #Bunker min amount
    @constraint(model, [f in 1:nFuel, i in bunkerPorts, s in 1:nScenarios], Ports[i].min_bunker - sum(y[i,f,s] for f in 1:nFuel) <= M1 * (1 - v[i,s]))


    # Loadline ports weight
    @constraint(model, [i in 1:nPort, s in 1:nScenarios], fullDraft[1] - (loadline[:Summer] - z - sum(b[i,f,s] for f in 1:nFuel)) / (tonPerCm[1] * 100) <= Ports[i].max_draft + M1 * (1 - v[i,s]))
    #Loadline routes draft
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z <= loadline[Routes[l].loadline[1]] + M1 * (1 - x[l,s]))
    @constraint(model, [l in 1:nRoute, k in 1:(Routes[l].nLegs - 1), s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z - consumption/speed*sum(Routes[l].distance[d] for d = 1:k) <= loadline[Routes[l].loadline[k+1]] + M1 * (1 - x[l,s]))
    #Loadline routeID draft
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], fullDraft[1] - (loadline[:Summer] - z - sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + consumption / speed * Routes[l].distToID) / (tonPerCm[1] * 100) <= routingRestriction[Routes[l].ID][2] + M1 * (1 - x[l,s]))
    #Loadline routeID weight
    @constraint(model, [l in 1:nRoute, s in 1:nScenarios], sum(b[Routes[l].or.nodeIndex,f,s] for f in 1:nFuel) + z - consumption / speed * Routes[l].distToID <= routingRestriction[Routes[l].ID][1] + M1 * (1 - x[l,s]) )
  
    
    # Time variable
    @constraint(model, [s in 1:nScenarios], t[s] == sum(x[l,s] * sum(Routes[l].distance) / speed for l in 1:nRoute) * seaMargin + sum(sum(y[i,f,s] for f in 1:nFuel) * Ports[i].time_quantity for i in 1:nPort) )

    #Test stuff
    #@constraint(model, [s in 1:nScenarios], sum(v[i,s] for i = 1:nPort)>=3)
    @constraint(model, [f in 1:nFuel, s in 1:nScenarios], y[start.nodeIndex, f, s] == o[f])

    optimize!(model)

    
    return model, -round(objective_value(model), digits=1), value(z), value.(x), value.(v), value.(b), value.(y), value.(t)
end

function main()
    #load_data("C:/Users/vikto/Documents/DTU/Thesis/Data.xlsx")
    graph, sm, rr = createInstance()
    scenarios = loadPrices(graph.nodes, 31)
    prices = [mean(subarray) for subarray in scenarios]
    scenariosFixed = scenarios #[i in [7,14] ? fill(mean(scenarios[i]), length(scenarios[i])) : scenarios[i] for i in eachindex(scenarios)]
    #scenariosFixed = [i in [7,14] ? fill(mean(scenarios[i]), length(scenarios[i])) : scenarios[i] for i in eachindex(scenarios)]
    for i in 1:size(graph.nodes,1)
        scenariosFixed[i] .= scenariosFixed[i] .+ 256
    end
    #println(scenariosFixed)


 #= k-means
    price_matrix1 = hcat(scenarios...)'
    hi = kmeans(price_matrix1, 20; maxiter=200)
    clusters1 = hi.centers
    clustered_elements = []
    for i in 1:14
        push!(clustered_elements, clusters1[i, :])
    end
    println(clustered_elements)
 =#

 #= k-medoids
    price_matrix = hcat(scenarios...)
    D = pairwise(Euclidean(), price_matrix; dims=1)
    kmedoids_result = kmedoids(D, 5; maxiter=200)
    clusters = kmedoids_result.medoids
    medoid_vectors = price_matrix[clusters, :]
    k_medoids = [medoid_vectors[:, i] for i in 1:size(medoid_vectors, 2)]
    println(k_medoids)
 =#

    #testing = [a[1,5,10,15,20] for a in scenarios]

    _, value2, z2, _, y2, t2, v2 = optimizeRouteTwoStage(graph, sm, rr, [subarray[1:365] for subarray in scenariosFixed])#  ###scenariosFixed)#
    #_, value, z, x, v, b, y, t = optimizeRouteTwoStageTest(graph, sm, rr, k_medoids)
    #_, c, g = optimizeRoute(graph, sm, rr, prices)
    #println(graph.nodes)
    #println(v)


 #=
    count = [0,0,0,0,0,0,0]
    cargo = []
    MGOorigin = []
    VLSFOorigin = []
    profit = []

    for i = 300:300
        prix = [subarray[i] for subarray in scenariosFixed]
        _, c, z, v, y, t = optimizeRoute(graph, sm, rr, prix)
        #_, c, z, b, y, t, v = optimizeRouteTwoStage(graph, sm, rr, prix)
        count = count .+ v
        push!(cargo, z)
        push!(MGOorigin, y[7,1])
        push!(VLSFOorigin, y[7,2])
        push!(profit, c)
        println(y, t)
    end

    println("Port visitations:", count)
    println("Max profit: ", maximum(profit))
    println("Min profit: ", minimum(profit))
    println("Avg profit: ", mean(profit))
    println("Max cargo: ", maximum(cargo))
    println("Min cargo: ", minimum(cargo))
    println("Avg cargo: ", mean(cargo))
    println("Max origin MGO: ", maximum(MGOorigin))
    println("Min origin MGO: ", minimum(MGOorigin))
    println("Avg origin MGO: ", mean(MGOorigin))
    println("Max origin VLSFO: ", maximum(VLSFOorigin))
    println("Min origin VLSFO: ", minimum(VLSFOorigin))
    println("Avg origin VLSFO: ", mean(VLSFOorigin))
    println("Profit combared to cont: ", mean(profit) - 2.6297493e6)


 =#
    println("2 stage obj: ", value2, " Route: ", v2, " Cargo: ", z2)
    println("Origin MGO: ", y2[7,1,1])
    println("Origin VLSFO: ", y2[7,2,1])
    println("Avg hire cost: ", mean(t2)*18000)
    #println(size(scenarios))
    println("Compared to base: ", 2.6297493e6 - value2)

 #

end

main()

end