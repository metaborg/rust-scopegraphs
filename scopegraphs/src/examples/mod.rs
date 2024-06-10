//! Examples on how to use scope graphs

use scopegraphs_render_docs::render_scopegraphs;

/// # Common patterns in scope graphs
pub mod patterns;

#[render_scopegrahs]
/// An example of a small language with records (structs) and name resolution of fields.
///
// {tutor name "records-example-full"}
/// ```
/// #
/// ```
// {tutor end name}
pub mod records {}
