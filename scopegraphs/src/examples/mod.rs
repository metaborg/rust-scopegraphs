//! Examples on how to use scope graphs

use scopegraphs_render_docs::render_scopegraphs;

/// # Common patterns in scope graphs
pub mod patterns;

#[render_scopegraphs]
/// An example of a small language with records (structs) and name resolution of fields.
///
/// ```
/// #
/// ```
pub mod records {}
