/*
 * Copyright 2021 Comcast Cable Communications Management, LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.automatics.rdkb.tests.webgui;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.utils.BroadBandPostConditionUtils;
import com.automatics.rdkb.utils.BroadBandPreConditionUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.webui.BroadBandWebUiBaseTest;
import com.automatics.rdkb.webui.page.LanSidePageNavigation;
import com.automatics.rdkb.webui.page.LanWebGuiLoginPage;
import com.automatics.tap.AutomaticsTapApi;

public class BroadBandPartnerWebGuiTest extends BroadBandWebUiBaseTest {

	boolean isBrowserOpen = false; // Boolean variable to check whether Browser is open.
	/** String for step number */
    public int stepNumber = 0;
    /**
     *
     * Test Case : Verify channel type, Upstream and Downstream information availability in admin login Public network
     * page for 2.4 Ghz
     *
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>PRE-CONDITION 1 : Connect the client setup to 2.4 GHz SSID and verify connection status</li>
     * <li>PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID</li>
     * <li>PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID</li>
     * <li>PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface</li>
     * <li>PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface</li>
     * <li>Step 1 : Verify login into the LAN GUI Admin page by using valid userid and valid password</li>
     * <li>Step 2 : Navigate to the Gateway>Connection>PUBLIC Network page and verify Navigation Status</li>
     * <li>Step 3 : Get the the downstream values in Public wifi network page</li>
     * <li>Step 4 : Verify the Lock Status value from downstream table in Public Network page and compare the Lock
     * Status value with webpa response *</li>
     * <li>Step 5 : Verify the Frequency value from downstream table in Public Network page and compare the Frequency
     * value with webpa response</li>
     * <li>Step 6 : Verify the SNR value from downstream table in Public Network page and compare the SNR value with
     * webpa response</li>
     * <li>Step 7 : Verify the Power Level value from downstream table in Public Network page compare the Power Level
     * value with webpa response</li>
     * <li>Step 8 : Verify the Modulation value from downstream table in Public Network page compare the Modulation
     * value with webpa response</li>
     * <li>Step 9 : Get the the upstream values in Public wifi network page</li>
     * <li>Step 10 : Verify the Lock Status value from upstream table in Public Network page compare the Lock Status
     * value with webpa response</li>
     * <li>Step 11 : Verify the Frequency value from upstream table in Public Network page compare the Frequency value
     * with webpa response</li>
     * <li>Step 12 : Verify the Symbol Rate from upstream table in Public Network page compare the Symbol Rate value
     * with webpa response</li>
     * <li>Step 13 : Verify the Power Level value from upstream table in Public Network page compare the Power Level
     * value with webpa response</li>
     * <li>Step 14 : Verify the Modulation value from upstream table in Public Network page compare the Modulation
     * value with webpa response</li>
     * <li>Step 15 : Verify the Channel Type value from upstream table in Public Network page compare the Channel Type
     * value with webpa response</li>
     * <li>POST-CONDITION 1 : Close the Browser LAN Client</li>
     * <li>POST-CONDITION 2 : Verify disconnecting the 2.4 GHz private wifi SSID</li>
     * </ol>
     * 
     * @param device
     *            {@link Dut}
     * @author Muthukumar
     * @refactor Rakesh C N
     */
    @Test(enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true)
    @TestDetails(testUID = "TC-RDKB-WIFI-2GHZ-PUBLIC-PAGE-5001")
    public void testToVerifyTheUpstreamAndDownStreamValuesInAdminLoginFor24Ghz(Dut device) {
	// Variable Declaration begins
	String testCaseId = "TC-RDKB-WIFI-2GHZ-PUBLIC-PAGE-501";
	String stepNum = null;
	String errorMessage = null;
	boolean status = false;
	LanSidePageNavigation lanSidePageNavigation = null;
	Dut deviceConnectedWith2Ghz = null;
	stepNumber = 1;
	// Variable Declation Ends
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2GHZ-PUBLIC-PAGE-5001");
	LOGGER.info(
		"TEST DESCRIPTION: Verify channel type, Upstream and Downstream information availability in admin login Public network page for 2.4 Ghz");
	LOGGER.info("TEST STEPS : ");
	LOGGER.info("PRE-CONDITION 1 : Connect  the  client setup to 2.4 GHz SSID and verify connection status");
	LOGGER.info("PRE-CONDITION 2 : Verify the correct IPv4 address for client connected with 2.4 GHz SSID");
	LOGGER.info("PRE-CONDITION 3 : Verify the correct IPv6 address for client connected with 2.4 GHz SSID");
	LOGGER.info(
		"PRE-CONDITION 4 : Verify the internet connectivity in the connected wifi client using ipv4 interface");
	LOGGER.info(
		"PRE-CONDITION 5 : Verify the internet connectivity in the connected wifi client using ipv6 interface");
	LOGGER.info("Step 1. Verify login into the LAN GUI Admin page by using valid userid and valid password");
	LOGGER.info("Step 2. Navigate to the Gateway>Connection>PUBLIC Network page and verify Navigation Status");
	LOGGER.info("Step 3 : Get the the downstream values in Public wifi network page");
	LOGGER.info(
		"Step 4 : Verify the Lock Status value from downstream table in Public Network page and compare the Lock Status value with webpa response  * ");
	LOGGER.info(
		"Step 5 : Verify the Frequency value from downstream table in Public Network page and compare the Frequency value with webpa response");
	LOGGER.info(
		"Step 6 : Verify the SNR value from downstream table in Public Network page and compare the SNR value with webpa response");
	LOGGER.info(
		"Step 7 : Verify the Power Level value from downstream table in Public Network page compare the Power Level value with webpa response");
	LOGGER.info(
		"Step 8 : Verify the Modulation value from downstream table in Public Network page compare the Modulation value with webpa response");
	LOGGER.info("Step 9 : Get the the upstream values in Public wifi network page");
	LOGGER.info(
		"Step 10 : Verify the Lock Status value from upstream table in Public Network page compare the Lock Status value with webpa response");
	LOGGER.info(
		"Step 11 : Verify the Frequency value from upstream table in Public Network page compare the Frequency value with webpa response");
	LOGGER.info(
		"Step 12 : Verify the Symbol Rate from upstream table in Public Network page compare the Symbol Rate value with webpa response");
	LOGGER.info(
		"Step 13 : Verify the Power Level value from upstream table in Public Network page compare the Power Level value with webpa response");
	LOGGER.info(
		"Step 14 : Verify the Modulation value from upstream table in Public Network page compare the Modulation value with webpa response");
	LOGGER.info(
		"Step 15 : Verify the Channel Type value from upstream table in Public Network page compare the Channel Type value with webpa response");
	LOGGER.info("POST-CONDITION 1 : Close the Browser LAN Client");
	LOGGER.info("POST-CONDITION 2 : Verify disconnecting the 2.4 GHz private wifi SSID");
	LOGGER.info("#######################################################################################");
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    deviceConnectedWith2Ghz = BroadBandPreConditionUtils.executePreConditionToVerifyWiFiClientStatus(device,
		    tapEnv, BroadBandTestConstants.BAND_2_4GHZ);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    /**
	     * Step 1 : VERIFY LOGIN INTO THE LAN GUI ADMIN PAGE BY USING VALID USERID AND VALID PASSWORD
	     */
	    BroadBandWebGuiTests.executeTestStepsToLoginAdminPage(device, deviceConnectedWith2Ghz, testCaseId, driver,
		    stepNumber);
	    isBrowserOpen = status;
	    driver = LanWebGuiLoginPage.getDriver();
	    LOGGER.info(" webDriver " + driver);
	    lanSidePageNavigation = new LanSidePageNavigation(driver);

	    /**
	     * STEP 2 : NAVIGATE TO THE GATEWAY > CONNECTION > PUBLIC NETWORK PAGE FOR RESIDENTIAL OR GATEWAY >
	     * CONNECTION > PUBLIC NETWORK PAGE FOR COMMERCIAL DEVICES AND VERIFY NAVIGATION STATUS
	     */
	    stepNumber++;
	    BroadBandWebGuiTests.executeTestStepsToLanuchPublicWiFiPage(device, lanSidePageNavigation, testCaseId,
		    driver, stepNumber);

	    /**
	     * SETP 3-15 : VLIDATING UPSTREAM AND DOWNSTREAM VALUES
	     */
	    stepNumber++;
	    BroadBandWebGuiTests.validateUpstreamDownsteramValuesInPublicPage(device, testCaseId, driver, stepNumber,
		    BroadBandTestConstants.STRING_VALUE_FIVE, BroadBandTestConstants.STRING_VALUE_SIX);
	} catch (Exception e) {
	    errorMessage = e.getMessage();
	    LOGGER.error("EXCEPTION OCCURRED WHILE VERIFYING UPSTREAM AND DOWNSTREAM VALUES IN LAN ADMIN 2.4 GHZ"
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, stepNum, status, errorMessage,
		    true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION STEPS");
	    /**
	     * Post-condition 1 : Close the LAN Side Browser
	     */
	    LOGGER.info("POST-CONDITION 1 : DESCRIPTION : VERIFY LAN SIDE BROWSER CLOSED");
	    LOGGER.info("POST-CONDITION 1 : ACTION : CLOSE THE LAN SIDE BROWSER");
	    LOGGER.info("POST-CONDITION 1 : EXPECTED : BROWSER CLOSED SUCCESSFULLY IN LAN SIDE");
	    if (isBrowserOpen) {
		try {
		    LanSidePageNavigation.closeBrowser();
		    LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER CLOSED SUCCESSFULLY.");
		} catch (Exception exception) {
		    LOGGER.info(
			    "POST-CONDITION 1 : ACTUAL : EXCEPTION OCCURRED WHILE CLOSING THE BROWSER, UNABLE TO CLOSE BROWSER.");
		}
	    } else {
		LOGGER.info("POST-CONDITION 1 : ACTUAL : BROWSER NOT OPENED IN LAN SIDE.");
	    }
	    /**
	     * Post-condition 2 : Disconnect the 2 GHz private wifi SSID
	     */
	    BroadBandPostConditionUtils.executePostConditionToDisconnectClientsConnectedWith5Ghz(device, tapEnv,
		    deviceConnectedWith2Ghz, BroadBandTestConstants.CONSTANT_2);
	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE:TC-RDKB-WIFI-2GHZ-PUBLIC-PAGE-5001");
    }
    
}

