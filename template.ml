let template page_title content =
  let open Tyxml.Html in
  let title = title (txt page_title) in
  let h1 = h1 [txt page_title] in
  html (head title []) (body (h1 :: content))
