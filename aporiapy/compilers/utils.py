name_id = 0


def generate_name(name="pred"):
    global name_id
    new_id = name_id
    name_id += 1
    return name + "_" + str(new_id)
