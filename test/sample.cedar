// Sample Cedar policies for testing cedar-ts-mode
// Covers all major language features

// Simple permit policy with annotations
@id("grant-photo-access")
@advice("Allow users to view their own photos")
permit (
  principal == User::"alice",
  action == Action::"viewPhoto",
  resource == Photo::"vacation.jpg"
);

// Forbid policy with `in` constraints and a condition
@id("deny-admin-unless-mfa")
forbid (
  principal in Group::"admins",
  action in [Action::"delete", Action::"modify"],
  resource
)
when {
  !context.mfa_authenticated
};

// Policy with `is` type constraints
@id("allow-employees")
permit (
  principal is Employee,
  action,
  resource is Document
)
when {
  principal.department == "engineering" &&
  resource.classification != "top-secret"
};

// Template policy with slots
permit (
  principal == ?principal,
  action,
  resource in ?resource
);

// Complex condition with if/then/else
@id("conditional-access")
permit (
  principal,
  action == Action::"read",
  resource
)
when {
  if principal.role == "manager"
  then resource.level <= 5
  else resource.level <= 2
};

// Policy with `unless` condition
@id("deny-weekend-access")
forbid (
  principal,
  action,
  resource
)
unless {
  context.day_of_week != "Saturday" &&
  context.day_of_week != "Sunday"
};

// has, like, and is expressions
@id("has-like-is-example")
permit (
  principal,
  action,
  resource
)
when {
  principal has department &&
  principal.name like "admin*" &&
  resource is Document &&
  resource has tags
};

// Extension function calls
@id("ip-restriction")
forbid (
  principal,
  action,
  resource
)
unless {
  ip(context.source_ip).isInRange(ip("10.0.0.0/8"))
};

// Method calls and contains expressions
@id("role-check")
permit (
  principal,
  action == Action::"approve",
  resource
)
when {
  principal.roles.contains("reviewer") &&
  principal.permissions.containsAll(resource.required_permissions)
};

// Set literal and record literal
@id("complex-literals")
permit (
  principal,
  action,
  resource
)
when {
  [1, 2, 3].contains(resource.priority) &&
  resource.metadata == {"author": "alice", "status": "published"}
};

// Arithmetic and comparisons
@id("numeric-checks")
permit (
  principal,
  action,
  resource
)
when {
  resource.size < 1000 * 1024 &&
  resource.count + 1 <= 100 &&
  resource.score >= 0 &&
  !(resource.priority > 5)
};

// Deeply nested entity paths
@id("nested-entities")
permit (
  principal == App::Service::Type::"backend",
  action in [
    App::Action::"read",
    App::Action::"list"
  ],
  resource in App::Folder::"shared"
);

// Decimal extension function
@id("decimal-example")
permit (
  principal,
  action,
  resource
)
when {
  decimal(resource.price).lessThan(decimal("100.00"))
};

// Bare principal/action/resource (match all)
permit (
  principal,
  action,
  resource
);
