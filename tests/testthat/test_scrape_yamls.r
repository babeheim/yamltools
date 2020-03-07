
# flat array with identical keys
list(
  list(
    date = "2017-01-01",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-02",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-03",
    amount = -3,
    notes = "blah"
  )
) %>% bind_rows -> interviews

# flat array, some unique keys
list(
  list(
    date = "2017-01-01",
    amount = -3,
    notes = "blah"
  ),
  list(
    date = "2017-01-02",
    amount = -3,
    notes = "blah",
    time = 1200
  ),
  list(
    date = "2017-01-03",
    amount = -3,
    notes = "blah",
    location = "home"
  )
) %>% bind_rows -> interviews

# array with one subtable, `entries`
list(
  list(
    interview_num = 1,
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  ),
  list(
    interview_num = 2,
    date = "2017-01-02",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  ),
  list(
    interview_num = 3,
    date = "2017-01-03",
    name = "alice",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  )
) -> x



# lets go simpler first

list(
  list(
    interview_num = 1,   # parent key of 'entries'
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        amount = -3,
        notes = "blah"
      ),
      list(
        amount = -5,
        notes = "blah2"
      ),
      list(
        amount = -10,
        notes = "blah blah"
      )
    )
  )
)

# becomes

list(
  list(
    interview_num = 1,
    date = "2017-01-01",
    name = "tom",
    entries = list(
      list(
        interview_num = 1,
        amount = -3,
        notes = "blah"
      ),
      list(
        interview_num = 1,
        amount = -5,
        notes = "blah2"
      ),
      list(
        interview_num = 1,
        amount = -10,
        notes = "blah blah"
      )
    )
  )
) -> x

# becomes

list(
  list(
    interview_num = 1,
    amount = -3,
    notes = "blah"
  ),
  list(
    interview_num = 1,
    amount = -5,
    notes = "blah2"
  ),
  list(
    interview_num = 1,
    amount = -10,
    notes = "blah blah"
  )
)
