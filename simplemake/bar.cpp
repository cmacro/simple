#include <stdlib.h>
#include <stdio.h>

#include "bar.h"

void Bar::SetBar(int bar)
{
	m_bar = bar;
}

void Bar::PutBar()
{
	char buf[16];
	snprintf(buf, sizeof(buf) - 1, "%d", m_bar);
	//setenv("BAR", buf, 1);
}