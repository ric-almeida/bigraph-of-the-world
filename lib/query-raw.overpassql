[timeout:36000];
relation(295355)->.boundary;
.boundary map_to_area->.a;

// PART 1: Get buildings
wr["building"](area.a)(around.boundary:0)->.buildings_on_boundary_unfiltered; // around is expensive, use only once
(
  wr.buildings_on_boundary_unfiltered["name"];
  wr.buildings_on_boundary_unfiltered["addr:street"]["addr:housenumber"];
)
->.buildings_on_boundary; // buildings that intersect the boundary

way(r.buildings_on_boundary)(area.a)
->.building_relations_way_members_inside; // way members of relations (buildings that intersect the boundary), filtered to those inside area
node(w.building_relations_way_members_inside)(area.a)
->.building_relations_way_members_nodes_inside; // nodes of relations'(buildings that intersect the boundary) way members, filtered to those inside area
(
  way.building_relations_way_members_inside(if:lrs_in("1",
    per_member(((pos()=="1")&&(lrs_in(ref(),building_relations_way_members_nodes_inside.set(id())))))));
  node(w.buildings_on_boundary)(area.a);
  node(r.buildings_on_boundary)(area.a);
)
->.buildings_members_inside; // members of relations and ways (buildings that intersect the boundary)

wr.buildings_on_boundary(if:!lrs_in("1",
    per_member(((pos()=="1")&&(lrs_in(ref(),buildings_members_inside.set(id())))))))
->.buildings_on_boundary_outside; // buildings that intersect the boundary to be excluded because the first member is outside

(
  (
    nwr["building"]["name"](area.a);
    nwr["building"]["addr:street"]["addr:housenumber"](area.a);
  ); - .buildings_on_boundary_outside;
)
->.named_buildings; // named buildings in the area


// PART 2: Get streets 
way["highway"~"^(motorway|trunk|primary|secondary|tertiary|unclassified|residential)(|_link)$"](area.a)
->.streets; // streets in the area


// PART 3: Get outer names
way["highway"~"^(motorway|trunk|primary|secondary|tertiary|unclassified|residential)(|_link)$"](area.a)(around.boundary:0)
->.streets_on_boundary; // streets that intersect the boundary
node(w.streets_on_boundary)(if:lrs_in(id(),streets_on_boundary.set(per_member((pos()=="1")?ref():0))))
->.outer_names; // outer names


// PART 4: Get child boundaries
(
  (
    relation["boundary"="administrative"](area.a);
    .boundary;
  );
  >>;
)
->.boundaries_in_area_and_members; // node, way and relation members of boundaries in the area

(
  node.boundaries_in_area_and_members(area.a); // node members of boundaries in area, filtered to those in the area
  way.boundaries_in_area_and_members(
    if:!lrs_in(
      "0",
      per_member(
        lrs_in(
          ref(),
          set(id()) // input set is nodes in the area
        )
      )
    )
  ); // way members of boundaries in area, filtered to those whose all nodes are in area
  .boundary >>;
);
complete // input set is node and way members in the area, 'complete' computes the transitive closure
{
  relation.boundaries_in_area_and_members(
    if:!lrs_in(
      "0",
      per_member(
        lrs_in(
          ref(),
          set(id()) // input set is the node, way and relation members of boundaries in the area
        )
      )
    )
  );
};
._->.boundaries_members_in_area; // transitive closure of the members of boundaries in the area

(
  relation.boundaries_members_in_area["boundary"="administrative"]; - .boundary;
)
->.child_boundaries;

(
  .named_buildings;
  .streets;
  .outer_names;
  .child_boundaries;
);
out meta;