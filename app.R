library("shiny")
library("gridlayout")
library("magick")
library("magrittr")
library("bslib")
library("tesseract")
library("sessioninfo")


ui <- grid_page(
    theme = bslib::bs_theme(version = 5, bootswatch = "united"),
    layout = c(
        "header   header   header",
        "sidebar  receiptValues   manualAdjust",
        "photoReceipt textReceipt howMuch"
    ),
    row_sizes = c(
        "100px",
        "2.7fr",
        "4fr"
    ),
    col_sizes = c(
        "1fr",
        "1fr",
        "1fr"
    ),
    gap_size = "1rem",
    grid_card(
        area = "sidebar",
        card_header("Receipt photo 📷"),
        card_body(
            fileInput("yourImage", NULL, accept = c(".png", ".jpg", ".jpeg")),
            helpText("Upload a receipt photo (or take one with your phone camera)."),
            numericInput(
                inputId = "imgRotation",
                label = "Image rotation angle",
                value = 7,
                min = 0,
                max = 359,
                step = 1,
                width = "100%"
            ),
            helpText("Change the angle to improve the text automatic scanning.")
        )
    ),
    grid_card_text(
        area = "header",
        content = "Split your receipt! 🧾",
        alignment = "start",
        is_title = TRUE
    ),
    grid_card(
        area = "howMuch",
        card_header("How much you owe 💸"),
        card_body(verbatimTextOutput(outputId = "amountYouOwe"))
    ),
    grid_card(
        area = "receiptValues",
        card_header("Figuring out how much you owe 🧮"),
        card_body(
            selectInput(
                inputId = "myItems",
                label = "Items you are paying for",
                choices = c(),
                multiple = TRUE
            ),
            helpText("Select 1 or more items"),
            selectInput(
                inputId = "receiptSubtotal",
                label = "Subtotal",
                choices = c(),
                multiple = FALSE
            ),
            selectInput(
                inputId = "receiptTotal",
                label = "Total",
                choices = c(),
                multiple = FALSE
            )
        )
    ),
    grid_card(
        area = "manualAdjust",
        card_header("Manual adjustments 🔎"),
        card_body(
            numericInput(
                inputId = "adjustItems",
                label = "Adjust your subtotal",
                value = 0
            ),
            helpText(
                "Useful if an item was not recongnized correctly from your receipt photo."
            ),
            numericInput(
                inputId = "adjustSubtotal",
                label = "Adjust overall subtotal",
                value = 0
            ),
            helpText(
                "Useful in cases where the subtotal includes tips, as in the example receipt: substract -7.72 to get the accurate subtotal (no taxes, no tips)."
            ),
            numericInput(
                inputId = "extraTip",
                label = "Extra tip",
                value = 0
            ),
            helpText(
                "Useful for the common case when you add extra tip: typically written by hand and not recognized as text in this website."
            )
        )
    ),
    grid_card(
        area = "textReceipt",
        card_header("Automatically scanned text 📝"),
        card_body(verbatimTextOutput(outputId = "receiptText")),
        helpText(
            "Only numbers with decimal values are shown as options in other menus."
        )
    ),
    grid_card(
        area = "photoReceipt",
        card_header("Processed receipt image 🖼️"),
        card_body(plotOutput(
            outputId = "processedReceipt",
            height = "800px"
        ))
    ),
    bslib::card_footer(
        HTML(
            "
            <hr />
            <p>This <a href='https://shiny.rstudio.com/'>shiny</a> application was developed by <a href='http://lcolladotor.github.io/'>Leonardo Collado Torres</a>. Check the code on <a href='https://github.com/lcolladotor/split_receipt'>GitHub</a>.</p>
        "
        )
    ),
    bslib::card_footer(
        HTML(
            "
        <hr />
        <center>
                <script type='text/javascript' id='clustrmaps' src='//cdn.clustrmaps.com/map_v2.js?cl=ffffff&w=300&t=n&d=D6u6SGD4V9Aj6RtXNpBFkBLHOVT4pH7YDiE7BMAN6q0'></script>
            </center>
        "
        )
    ),
    bslib::card_footer(
        hr(),
        helpText(
            "This website is meant to help you figure out how much you owe when you split a restaurant / bar bill with your colleagues and/or friends. It makes the simplifying assumption that taxes (and tips) will be split proportionally to what you consumed. For instance, in Maryland (USA) food and alcohol taxes are not the same: it's 6% for food (sales in general) and 9% for alcohol items. If you want to split bills precisely you would need to know what type of tax was applied for every item in a receipt, which is painful. As the difference is typically small, doing the precise math is not needed in general. Thus you can figure out how much you owe by calculating the proportion of the subtotal that you consumed and then multiplying the overall total (after taxes and tips) by that proportion. Then round it off to two decimals."
        )
    ),
    tags$head(tags$link(rel="icon", href="https://github.com/lcolladotor/lcolladotorsource/blob/master/assets/media/icon.png?raw=true"))
)


server <- function(input, output) {
    # myCamera <- callModule(
    #     shinyviewr, "my_camera",
    #     output_width = 250,
    #     output_height = 250
    # )

    img <- reactive({
        # if(is.null(myCamera())) {
        #     image_read("/a3080d75-fcb0-4b6c-8132-eb69c2c50199.JPG")
        # }
        if (!is.null(input$yourImage)) {
            image_read(input$yourImage$datapath)
        } else {
            image_read("a3080d75-fcb0-4b6c-8132-eb69c2c50199.JPG")
        }
    })

    observeEvent(input$yourImage, {
        updateNumericInput(inputId = "imgRotation", value = 0)
    })


    observeEvent(input$imgRotation, {
        img_mod <- img() %>%
            image_deskew() %>%
            image_rotate(input$imgRotation) %>%
            image_convert(type = "Grayscale") %>%
            image_trim(fuzz = 40)

        text <- img_mod %>%
            image_write(format = "png", density = "300x300") %>%
            tesseract::ocr()

        output$processedReceipt <- renderPlot({
            plot(as.raster(img_mod))
        })

        output$receiptText <- renderPrint({
            cat(text)
        })

        numbers_in_text <-
            as.numeric(gsub(
                "\\,",
                ".",
                stringr::str_extract_all(text, "[0-9]+(\\.|\\,)[0-9]+")[[1]]
            ))

        updateSelectInput(
            inputId = "myItems",
            choices = numbers_in_text,
            selected = NULL
        )
        updateSelectInput(
            inputId = "receiptSubtotal",
            choices = sort(unique(numbers_in_text), decreasing = TRUE),
            selected = sort(unique(numbers_in_text), decreasing = TRUE)[2]
        )
        updateSelectInput(
            inputId = "receiptTotal",
            choices = sort(unique(numbers_in_text), decreasing = TRUE),
            selected = max(numbers_in_text)
        )

        output$amountYouOwe <- renderPrint({
            subtotal_yours <-
                sum(as.numeric(input$myItems), na.rm = TRUE) + input$adjustItems
            subtotal_all <-
                sum(as.numeric(input$receiptSubtotal), na.rm = TRUE) +
                input$adjustSubtotal
            total_all <-
                sum(as.numeric(input$receiptTotal), na.rm = TRUE) + input$extraTip

            cat(
                c(
                    " Your subtotal: ",
                    subtotal_yours,
                    "\n",
                    "Overall subtotal: ",
                    subtotal_all,
                    "\n",
                    "Proportion you are paying: ",
                    round(subtotal_yours / subtotal_all, 4),
                    "\n",
                    "Total (with extra tip): ",
                    total_all,
                    "\n",
                    "-----------------\n",
                    "You owe: ",
                    round(subtotal_yours / subtotal_all * total_all, 2),
                    "\n"
                )
            )
        })
    })
}

## Reproducibility info
options(width = 120)
print(sessioninfo::session_info())

shinyApp(ui, server)
