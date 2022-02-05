use std::io::{self, Write};

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
}

pub trait XmlWriter {
    fn write_xml(&mut self, elm: &Element) -> io::Result<()>;
    fn write_with_indent(&mut self, elm: &Element, indent_level: usize) -> io::Result<()>;
}

impl<W: Write> XmlWriter for W {
    fn write_xml(&mut self, elm: &Element) -> io::Result<()> {
        self.write_with_indent(elm, 0)
    }

    fn write_with_indent(&mut self, elm: &Element, indent_level: usize) -> io::Result<()> {
        use Content::*;
        let indent = "  ".repeat(indent_level);
        match &elm.content {
            Empty => (),
            Elements(elements) => {
                writeln!(self, "{}<{}>", indent, elm.name)?;
                for elm in elements {
                    self.write_with_indent(elm, indent_level + 1)?;
                }
                writeln!(self, "{}</{}>", indent, elm.name)?;
            }
            Fragment(elements) => {
                for elm in elements {
                    self.write_with_indent(elm, indent_level)?;
                }
            }
            Text(text) => {
                writeln!(self, "{}<{}> {} </{1}>", indent, elm.name, text)?;
            }
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::analyzer::xml::{Element, XmlWriter};
    use std::io::Cursor;

    fn get_xml_string(elm: &Element) -> String {
        let mut cursor = Cursor::new(Vec::new());
        cursor.write_xml(elm).unwrap();
        cursor
            .into_inner()
            .iter()
            .map(|&s| s as char)
            .collect::<String>()
    }

    #[test]
    fn create_joinned_fragmen() {
        let elm = Element::new_elements(
            "parent",
            vec![
                Element::new_joinned_fragment(
                    vec![Element::new_text("a", "text1")],
                    Element::new_text("symbol", ","),
                ),
                Element::new_joinned_fragment(
                    vec![
                        Element::new_text("aa", "text1"),
                        Element::new_text("bb", "text2"),
                    ],
                    Element::new_text("symbol", ","),
                ),
                Element::new_joinned_fragment(
                    vec![
                        Element::new_text("aaa", "text1"),
                        Element::new_text("bbb", "text2"),
                        Element::new_text("ccc", "text3"),
                    ],
                    Element::new_text("symbol", ","),
                ),
            ],
        );
        let actual = get_xml_string(&elm);

        assert_eq!(
            actual,
            "<parent>
  <a> text1 </a>
  <aa> text1 </aa>
  <symbol> , </symbol>
  <bb> text2 </bb>
  <aaa> text1 </aaa>
  <symbol> , </symbol>
  <bbb> text2 </bbb>
  <symbol> , </symbol>
  <ccc> text3 </ccc>
</parent>
"
        );
    }

    #[test]
    fn write() {
        let elm = Element::new_element(
            "expression",
            Element::new_element("term", Element::new_text("keyword", "true")),
        );
        let actual = get_xml_string(&elm);

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
