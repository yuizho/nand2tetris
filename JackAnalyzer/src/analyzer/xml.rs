use std::io::{self, Write};

const INDENT_SPACE_COUNT: usize = 2;

#[derive(PartialEq, Eq, Debug, Clone)]
pub struct Element {
    name: String,
    content: Content,
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Content {
    Empty,
    Fragment(Vec<Element>),
    Elements(Vec<Element>),
    Text(String),
}

impl Element {
    pub fn new_element<S: Into<String>>(name: S, child: Element) -> Self {
        Element {
            name: name.into(),
            content: Content::Elements(vec![child]),
        }
    }

    pub fn new_elements<S: Into<String>>(name: S, children: Vec<Element>) -> Self {
        Element {
            name: name.into(),
            content: Content::Elements(children),
        }
    }

    pub fn new_fragment(children: Vec<Element>) -> Self {
        Element {
            name: "".to_string(),
            content: Content::Fragment(children),
        }
    }

    pub fn new_joinned_fragment(children: Vec<Element>, separator: Element) -> Self {
        let mut joinned = vec![];
        for (i, child) in children.iter().enumerate() {
            if i > 0 && i < children.len() {
                joinned.push(separator.clone());
            }
            joinned.push(child.clone());
        }
        Self::new_fragment(joinned)
    }

    pub fn new_text<S: Into<String>>(name: S, text: S) -> Self {
        Element {
            name: name.into(),
            content: Content::Text(text.into()),
        }
    }

    pub fn empty() -> Self {
        Element {
            name: "".to_string(),
            content: Content::Empty,
        }
    }

    pub fn write<W: Write>(&self, writer: &mut W) -> io::Result<()> {
        self.write_with_indent(writer, 0)
    }

    fn write_with_indent<W: Write>(&self, writer: &mut W, level: usize) -> io::Result<()> {
        use Content::*;
        let prefix = " ".repeat(level);
        match &self.content {
            Empty => (),
            Elements(elements) => {
                writeln!(writer, "{}<{}>", prefix, self.name)?;
                for elm in elements {
                    elm.write_with_indent(writer, level + INDENT_SPACE_COUNT)?;
                }
                writeln!(writer, "{}</{}>", prefix, self.name)?;
            }
            Fragment(elements) => {
                for elm in elements {
                    elm.write_with_indent(writer, level)?;
                }
            }
            Text(text) => {
                writeln!(writer, "{}<{}> {} </{1}>", prefix, self.name, text)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::xml::Element;
    use std::io::Cursor;

    #[test]
    fn write() {
        let elm = Element::new_element(
            "expression",
            Element::new_element("term", Element::new_text("keyword", "true")),
        );

        let mut cursor = Cursor::new(Vec::new());
        elm.write(&mut cursor).unwrap();

        let actual = cursor
            .into_inner()
            .iter()
            .map(|&s| s as char)
            .collect::<String>();
        assert_eq!(
            actual,
            "<expression>
  <term>
    <keyword> true </keyword>
  </term>
</expression>
"
        );
    }
}
