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
package com.automatics.rdkb.tests.parentalcontrol;

import java.util.ArrayList;
import java.util.List;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Dut;
import com.automatics.rdkb.BroadBandParentalControlParameter;
import com.automatics.rdkb.BroadBandResultObject;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants.ParentalControlBlockMethod;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.parentalcontrol.BroadBandParentalControlUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.webpa.WebPaServerResponse;

public class BroadBandBlockSiteTest extends AutomaticsTestBase{
    /**
     * Test to Verify the internet access to the sites and keywords added to manages sites are blocked for a specific
     * day and time period in Wi-Fi and LAN clients
     * 
     * <ol>
     * <li>STEP 1: Verify the Parental Control - Manage Sites can be enabled via WebPA</li>
     * <li>STEP 2: Verify the website url which needs to be blocked for a specific day and time period on connected
     * clients can be added to blocked sites table</li>
     * <li>STEP 3: Verify the website added to blocked sites table is listed in iptables</li>
     * <li>STEP 4: Verify the website added to blocked sites table is listed in ip6tables</li>
     * <li>STEP 5: Verify the keyword which needs to be blocked for a specific day and time period on connected clients
     * can be added to blocked sites table</li>
     * <li>STEP 6: Verify the keyword added to blocked sites table is listed in iptables</li>
     * <li>STEP 7: Verify the keyword added to blocked sites table is listed in ip6tables</li>
     * <li>STEP 8: Verify whether Private 2.4 GHz and 5 GHz SSIDs' are enabled using WebPA</li>
     * <li>STEP 9: Connect the device to 2.4 GHz SSID and verify connection status</li>
     * <li>STEP 10: Check if the wireless connected client has an IP address from the gateway</li>
     * <li>STEP 11: Verify the blocked website url can not be accessible at a specified day and time period from the
     * client connected to 2.4 GHz Wi-Fi Network</li>
     * <li>STEP 12: Verify the keyword added to blocked list can not be accessible at a specified day and time period
     * from the client connected to 2.4 GHz Wi-Fi Network</li>
     * <li>STEP 13: Connect the device to 5 GHz SSID and verify connection status</li>
     * <li>STEP 14: Check if the wireless connected client has an IP address from the gateway</li>
     * <li>STEP 15: Verify the blocked website url can not be accessible at a specified day and time period from the
     * client connected to 5 GHz Wi-Fi Network</li>
     * <li>STEP 16: Verify the keyword added to blocked list can not be accessible at a specified day and time period
     * from the client connected to 5 GHz Wi-Fi Network</li>
     * <li>STEP 17: Verify the client connected to ethernet has Internet Access</li>
     * <li>STEP 18: Verify the blocked website url can not be accessible at a specified day and time period from the
     * client connected to Ethernet</li>
     * <li>STEP 19: Verify the keyword added to blocked list can not be accessible at a specified day and time period
     * from the client connected to Ethernet</li>
     * <li>POST-CONDITION 1: Verify the sites and keywords added to blocked sites table are removed</li>
     * <li>POST-CONDITION 2: Verify the Parental Control - Manage Sites is disabled via WebPA</li>
     * </ol>
     * 
     * @param device
     * @refactor Govardhan
     */

    @Test(alwaysRun = true, enabled = true, dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class)
    @TestDetails(testUID = "TC-RDKB-PC-MNG-SITE-1001")
    public void testToBlockSiteForParticularTimePeriodAndCheckInternetAccess(Dut device) {
	String testId = "TC-RDKB-PC-MNG-SITE-101";
	String testStepNumber = "s1";
	String errorMessage = null;
	boolean status = false;
	boolean isEnabled = false;
	Dut connectedClientSettop = null; // connected device to be verified
	BroadBandResultObject result = null; // stores test result and error message
	List<String> newRowsAdded = new ArrayList<String>();

	String parentalControlManageSiteTableAddRowResponse = null;
	BroadBandParentalControlParameter parentalControlParam = null;
	String blockDays = null;
	String ipTableResponse = null;
	String ip6TableResponse = null;
	try {

	    LOGGER.info("#################### STARTING TEST CASE: TC-RDKB-PC-MNG-SITE-1001 #####################");
	    LOGGER.info(
		    "TEST DESCRIPTION: Verify the internet access to the sites and keywords added to manages sites are blocked for a specific day and time period in Wi-Fi and LAN clients");

	    LOGGER.info("TEST STEPS : ");

	    LOGGER.info("1: Verify the Parental Control - Managed Sites can be enabled via WebPA");
	    LOGGER.info(
		    "2: Verify the website url which needs to be blocked for a specific day and time period on connected clients can be added to blocked sites table");
	    LOGGER.info("3: Verify the website added to blocked sites table is listed in iptables");
	    LOGGER.info("4: Verify the website added to blocked sites table is listed in ip6tables");
	    LOGGER.info(
		    "5: Verify the keyword which needs to be blocked for a specific day and time period  on connected clients can be added to blocked sites table");
	    LOGGER.info("6: Verify the keyword added to blocked sites table is listed in iptables");
	    LOGGER.info("7: Verify the keyword added to blocked sites table is listed in ip6tables");
	    LOGGER.info("8: Verify whether Private 2.4 GHz and 5 GHz SSIDs' are enabled using WebPA");
	    LOGGER.info("9: Connect the device to 2.4 GHz SSID and verify connection status");
	    LOGGER.info("10: Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "11: Verify the blocked website url can not be accessible at a specified day and time period from the client connected to 2.4 GHz Wi-Fi Network");
	    LOGGER.info(
		    "12: Verify the keyword added to blocked list can not be accessible at a specified day and time period  from the client connected to 2.4 GHz Wi-Fi Network");
	    LOGGER.info("13: Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("14: Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "15: Verify the blocked website url can not be accessible at a specified day and time period from the client connected to 5 GHz Wi-Fi Network");
	    LOGGER.info(
		    "16: Verify the keyword added to blocked list can not be accessible at a specified day and time period from the client connected to 5 GHz Wi-Fi Network");
	    LOGGER.info("17: Verify the client connected to ethernet has Internet Access");
	    LOGGER.info(
		    "18: Verify the blocked website url can not be accessible at a specified day and time period from the client connected to Ethernet");
	    LOGGER.info(
		    "19: Verify the keyword added to blocked list can not be accessible at a specified day and time period from the client connected to  Ethernet");
	    LOGGER.info("POST-CONDITION 1: Verify the sites and keywords added to blocked sites table are removed");
	    LOGGER.info("POST-CONDITION 2: Verify the Parental Control-Manage Sites is disabled via WebPA");
	    LOGGER.info("#####################################################################################");

	    /**
	     * Step 1: Verify the Parental Control - Managed Sites can be enabled via WebPA
	     *
	     */
	    testStepNumber = "s1";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 1: DESCRIPTION: Verify the Parental Control - Manage Sites can be enabled via WebPA");
	    LOGGER.info("STEP 1: EXPECTED: Managed Sites should be enabled via WebPA");
	    status = BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROLS, WebPaDataTypes.BOOLEAN.getValue(),
		    BroadBandTestConstants.TRUE);
	    isEnabled = status;
	    errorMessage = "Parental Control - Manage Sites cannot be enabled";
	    if (status) {
		LOGGER.info("STEP 1: ACTUAL: Parental Control - Manage Site is enabled via WebPA");
	    } else {
		LOGGER.error("STEP 1: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 2: Verify the website which needs to be blocked for a specific day and time period on connected
	     * clients can be added to blocked sites table
	     * 
	     */
	    testStepNumber = "s2";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 2: DESCRIPTION: Verify the website which needs to be blocked for a specific day and time period on connected clients can be added to blocked sites table");
	    LOGGER.info("STEP 2: EXPECTED: Website should be added successfully to blocked sites table");

	    blockDays = BroadBandParentalControlUtils
		    .getCurrentDayToAddParentalControlRuleWhenAlwaysBlockIsDisabled(tapEnv, device);
	    errorMessage = "Null value obtained for the current day which needs to added in parental Control - Managed Device Rule. Actual value Obtained: "
		    + blockDays;
	    if (CommonMethods.isNotNull(blockDays)) {
		parentalControlParam = new BroadBandParentalControlParameter(ParentalControlBlockMethod.URL.getValue(),
			BroadBandTestConstants.URL_WIKIPEDIA, BroadBandTestConstants.FALSE,
			BroadBandTestConstants.HOURS_12_AM, BroadBandTestConstants.HOURS_23_59_PM, blockDays);
		parentalControlManageSiteTableAddRowResponse = BroadBandCommonUtils.setParentalControlParam(device,
			tapEnv, parentalControlParam);
		errorMessage = "Null response obtained for setting Parental Control - Managed Sites Rule.";
		if (CommonMethods.isNotNull(parentalControlManageSiteTableAddRowResponse)) {
		    status = CommonUtils.patternSearchFromTargetString(parentalControlManageSiteTableAddRowResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_A_PARENTAL_CONTROL_RULE);
		    errorMessage = "Website which needs to be blocked for a specific day and time period on the connected client cannot be added to blocked sites table.";
		}
	    }

	    if (status) {
		newRowsAdded.add(parentalControlManageSiteTableAddRowResponse);
		LOGGER.info(
			"STEP 2: ACTUAL: Website which needs to be blocked for a specific day and time period on the client connected to the gateway is added succesfully to blocked sites table");
	    } else {
		LOGGER.error("STEP 2: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: "
			+ parentalControlManageSiteTableAddRowResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 3: Verify the website added to blocked sites table is listed in iptables
	     *
	     */
	    testStepNumber = "s3";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 3: DESCRIPTION: Verify the website added to blocked sites table is listed in iptables");
	    LOGGER.info(
		    "STEP 3: EXPECTED: Website added to blocked sites table should be listed in iptables as blocked");
	    LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    ipTableResponse = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE, BroadBandTestConstants.WIKIPEDIA));
	    errorMessage = "Null response received on obtaining values from iptables.";
	    if (CommonMethods.isNotNull(ipTableResponse)) {
		status = CommonUtils.patternSearchFromTargetString(ipTableResponse, BroadBandTestConstants.WIKIPEDIA)
			&& CommonUtils.patternSearchFromTargetString(ipTableResponse,
				BroadBandTestConstants.SITE_BLOCKED);
		errorMessage = "Website added to blocked sites table is not listed in iptables.";
	    }
	    if (status) {
		LOGGER.info("STEP 3: ACTUAL: Website added to blocked sites table is listed in iptables as blocked");
	    } else {
		LOGGER.error("STEP 3: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ipTableResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 4: Verify the website added to blocked sites table is listed in ip6tables
	     *
	     */
	    testStepNumber = "s4";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 4: DESCRIPTION: Verify the website added to blocked sites table is listed in ip6tables");
	    LOGGER.info(
		    "STEP 4: EXPECTED: Website added to blocked sites table should be listed in ip6tables as blocked");
	    LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    ip6TableResponse = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES, BroadBandTestConstants.WIKIPEDIA));
	    errorMessage = "Null response received on obtaining values from ip6tables.";
	    if (CommonMethods.isNotNull(ip6TableResponse)) {
		status = CommonUtils.patternSearchFromTargetString(ip6TableResponse, BroadBandTestConstants.WIKIPEDIA)
			&& CommonUtils.patternSearchFromTargetString(ip6TableResponse,
				BroadBandTestConstants.SITE_BLOCKED);
		errorMessage = "Website added to blocked sites table is not listed in ip6tables.";
	    }
	    if (status) {
		LOGGER.info("STEP 4: ACTUAL: Website added to blocked sites table is listed in ip6tables as blocked");
	    } else {
		LOGGER.error("STEP 4: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ip6TableResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 5: Verify the keyword which needs to be blocked for a specific day and time period on connected
	     * clients can be added to blocked sites table
	     *
	     */
	    testStepNumber = "s5";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 5: DESCRIPTION: Verify the keyword which needs to be blocked for a specific day and time period  on connected clients can be added to blocked sites table");
	    LOGGER.info("STEP 5: EXPECTED: Keyword should be added successfully to blocked sites table");

	    blockDays = BroadBandParentalControlUtils
		    .getCurrentDayToAddParentalControlRuleWhenAlwaysBlockIsDisabled(tapEnv, device);
	    errorMessage = "Null value obtained for the current day which needs to added in parental Control - Managed Device Rule. Actual value Obtained: "
		    + blockDays;
	    if (CommonMethods.isNotNull(blockDays)) {
		parentalControlParam = new BroadBandParentalControlParameter(
			ParentalControlBlockMethod.Keyword.getValue(), BroadBandTestConstants.INSTAGRAM,
			BroadBandTestConstants.FALSE, BroadBandTestConstants.HOURS_12_AM,
			BroadBandTestConstants.HOURS_23_59_PM, blockDays);
		parentalControlManageSiteTableAddRowResponse = BroadBandCommonUtils.setParentalControlParam(device,
			tapEnv, parentalControlParam);
		errorMessage = "Null response obtained for setting Parental Control - Managed Site Rule.";
		if (CommonMethods.isNotNull(parentalControlManageSiteTableAddRowResponse)) {
		    status = CommonUtils.patternSearchFromTargetString(parentalControlManageSiteTableAddRowResponse,
			    BroadBandWebPaConstants.WEBPA_PARAM_TO_SET_A_PARENTAL_CONTROL_RULE);
		    errorMessage = "Keyword which needs to be blocked for a specific day and time period on the connected client cannot be added to blocked sites table.";
		}
	    }
	    if (status) {
		newRowsAdded.add(parentalControlManageSiteTableAddRowResponse);
		LOGGER.info(
			"STEP 5: ACTUAL: Keyword which needs to be blocked for a specific day and time period on the client connected to the gateway is added succesfully to blocked sites table");
	    } else {
		LOGGER.error("STEP 5: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: "
			+ parentalControlManageSiteTableAddRowResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 6: Verify the keyword added to blocked sites table is listed in iptable
	     *
	     */
	    testStepNumber = "s6";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 6: DESCRIPTION: Verify the keyword added to blocked sites table is listed in iptables");
	    LOGGER.info(
		    "STEP 6: EXPECTED: Keyword added to blocked sites table should be listed in iptables as blocked");
	    LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    ipTableResponse = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IPTABLE, BroadBandTestConstants.INSTAGRAM));
	    errorMessage = "Null response received on obtaining values from iptables.";
	    if (CommonMethods.isNotNull(ipTableResponse)) {
		status = CommonUtils.patternSearchFromTargetString(ipTableResponse, BroadBandTestConstants.INSTAGRAM)
			&& CommonUtils.patternSearchFromTargetString(ipTableResponse,
				BroadBandTestConstants.SITE_BLOCKED);
		errorMessage = "Keyword added to blocked sites table is not listed in iptables";
	    }
	    if (status) {
		LOGGER.info("STEP 6: ACTUAL: Keyword added to blocked sites table is listed in iptables as blocked");
	    } else {
		LOGGER.error("STEP 6: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ipTableResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 7: Verify the keyword added to blocked sites table is listed in ip6tables
	     *
	     */
	    testStepNumber = "s7";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 7: DESCRIPTION: Verify the keyword added to blocked sites table is listed in ip6tables");
	    LOGGER.info(
		    "STEP 7: EXPECTED: Keyword added to blocked sites table should be listed in ip6tables as blocked");
	    LOGGER.info("################## Waiting for 30 seconds to reflect the changes ####################");
	    tapEnv.waitTill(BroadBandTestConstants.THIRTY_SECOND_IN_MILLIS);
	    ip6TableResponse = tapEnv.executeCommandUsingSsh(device, BroadBandCommonUtils.concatStringUsingStringBuffer(
		    BroadBandCommandConstants.CMD_TO_GREP_VALUES_FROM_IP6TABLES, BroadBandTestConstants.INSTAGRAM));
	    errorMessage = "Null response received on obtaining values from ip6tables.";
	    if (CommonMethods.isNotNull(ip6TableResponse)) {
		status = CommonUtils.patternSearchFromTargetString(ip6TableResponse, BroadBandTestConstants.INSTAGRAM)
			&& CommonUtils.patternSearchFromTargetString(ip6TableResponse,
				BroadBandTestConstants.SITE_BLOCKED);
		errorMessage = "Keyword added to blocked sites table is not listed in ip6tables";
	    }
	    if (status) {
		LOGGER.info("STEP 7: ACTUAL: Keyword added to blocked sites table is listed in ip6tables as blocked");
	    } else {
		LOGGER.error("STEP 7: ACTUAL: " + errorMessage + " ACTUAL RESPONSE: " + ip6TableResponse);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 8: Verify whether Private 2.4 GHz & 5 GHz SSIDs' are enabled using WebPA
	     *
	     */
	    testStepNumber = "s8";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 8: DESCRIPTION: Verify whether Private 2.4 GHz & 5 GHz SSIDs' are enabled using WebPA");
	    LOGGER.info("STEP 8: EXPECTED: Both 2.4 GHz & 5 GHz private Wi-Fi radios should be enabled");
	    status = BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
		    WiFiFrequencyBand.WIFI_BAND_2_GHZ, tapEnv, device, true)
		    && BroadBandConnectedClientUtils.enableOrDisableRadiosForGivenSsidUsingWebPaCommand(
			    WiFiFrequencyBand.WIFI_BAND_5_GHZ, tapEnv, device, true);
	    errorMessage = "Enabling private Wi-Fi SSIDs' via WebPA failed";
	    if (status) {
		LOGGER.info("STEP 8: ACTUAL: Both 2.4 GHz & 5 GHz Private Wi-Fi SSIDs' are enabled using WebPA");
	    } else {
		LOGGER.error("STEP 8: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 9: Connect the device to 2.4 GHz SSID and verify connection status
	     * 
	     */
	    testStepNumber = "s9";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 9: DESCRIPTION: Connect the device to 2.4 GHz SSID and verify connection status");
	    LOGGER.info("STEP 9: EXPECTED: Device should be connected with 2.4 GHz wifi network");
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    status = null != connectedClientSettop;
	    errorMessage = "Unable to connect to 2.4 GHz private Wi-Fi Network Or 2.4 GHz WiFi capable devices are not available";
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 9: ACTUAL: Device has been connected with 2.4 GHz private Wi-Fi network");
	    } else {
		LOGGER.error("STEP 9: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 10: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    testStepNumber = "s10";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 10: DESCRIPTION: Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "STEP 10: EXPECTED: DHCP Range IP Address should be assigned to 2.4 GHz Wireless Connected device");
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedClientSettop);
	    errorMessage = "Cilent connected to 2.4 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(
			"STEP 10: ACTUAL: Client connected to 2.4 Ghz private Wi-Fi network has got IP Address from Gateway");
	    } else {
		LOGGER.error("STEP 10: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 11: Verify the blocked website url cannot be accessible from the client connected to 2.4 GHz Wi-Fi
	     * Network
	     *
	     */
	    testStepNumber = "s11";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 11: DESCRIPTION: Verify the blocked website cannot be accessible from the client connected to 2.4 GHz Wi-Fi Network");
	    LOGGER.info(
		    "STEP 11: EXPECTED: Blocked website should not be accessible from connected client 2.4 GHz private Wi-Fi Network");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_WIKIPEDIA);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 11: ACTUAL: Blocked Website is not accessible in the client connected to 2.4 Ghz private Wi-Fi network");
	    } else {
		LOGGER.error("STEP 11: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 12: Verify the keyword added to blocked list can not be accessible from the client connected to 2.4
	     * GHz Wi-Fi Network
	     *
	     */
	    testStepNumber = "s12";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 12: DESCRIPTION: Verify the keyword added to blocked list can not be accessible from the client connected to 2.4 GHz Wi-Fi Network");
	    LOGGER.info(
		    "STEP 12: EXPECTED: Blocked keyword should not be accessible from connected client 2.4 GHz private Wi-Fi Network");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_INSTAGRAM);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 12: ACTUAL: Keyword added to blocked site list is not accessible in the client connected to 2.4 Ghz private Wi-Fi network, hence blocked");
	    } else {
		LOGGER.error("STEP 12: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 13: Connect the device to 5 GHz SSID and verify connection status
	     * 
	     */
	    testStepNumber = "s13";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 13: DESCRIPTION: Connect the device to 5 GHz SSID and verify connection status");
	    LOGGER.info("STEP 13: EXPECTED: Device should be connected with 5 GHz wifi network");
	    connectedClientSettop = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    status = null != connectedClientSettop;
	    errorMessage = "Unable to connect to 5 GHz private Wi-Fi Network Or 5 GHz WiFi capable devices are not available";
	    if (status) {
		LOGGER.info("GOING TO WAIT FOR 2 MINUTES AFTER CONNECTING THE WIFI CLIENT.");
		tapEnv.waitTill(BroadBandTestConstants.TWO_MINUTE_IN_MILLIS);
		LOGGER.info("STEP 13: ACTUAL: Device has been connected with 5 GHz private Wi-Fi network");
	    } else {
		LOGGER.error("STEP 13: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 14: Check if the wireless connected client has an IP address from the gateway
	     *
	     */
	    testStepNumber = "s14";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 14: DESCRIPTION: Check if the wireless connected client has an IP address from the gateway");
	    LOGGER.info(
		    "STEP 14: EXPECTED: DHCP Range IP Address should be assigned to 5 GHz Wireless Connected device");
	    status = BroadBandConnectedClientUtils.verifyIpv4AddressOFConnectedClientIsBetweenDhcpRange(tapEnv, device,
		    connectedClientSettop);
	    errorMessage = "Cilent connected to 5 GHz private Wi-Fi haven't received valid IP Address from Gateway";
	    if (status) {
		LOGGER.info(
			"STEP 14: ACTUAL: Client connected to 5 Ghz private Wi-Fi network has got IP Address from Gateway");
	    } else {
		LOGGER.error("STEP 14: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 15: Verify the blocked website url cannot be accessible from the client connected to 5 GHz Wi-Fi
	     * Network
	     *
	     */
	    testStepNumber = "s15";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 15: DESCRIPTION: Verify the blocked website url cannot be accessible from the client connected to 5 GHz Wi-Fi Network");
	    LOGGER.info(
		    "STEP 15: EXPECTED: Blocked website should not be accessible from connected client 5 GHz private Wi-Fi Network");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_WIKIPEDIA);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 15: ACTUAL: Blocked Website is not accessible in the client connected to 5 Ghz private Wi-Fi network");
	    } else {
		LOGGER.error("STEP 15: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 16: Verify the keyword added to blocked list can not be accessible from the client connected to 5
	     * GHz Wi-Fi Network
	     *
	     */
	    testStepNumber = "s16";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 16: DESCRIPTION: Verify the keyword added to blocked list can not be accessible from the client connected to 5 GHz Wi-Fi Network");
	    LOGGER.info(
		    "STEP 16: EXPECTED: Blocked keyword should not be accessible from connected client 5 GHz private Wi-Fi Network");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_INSTAGRAM);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 16: ACTUAL: Keyword added to blocked site list is not accessible in the client connected to 5 Ghz private Wi-Fi network, hence blocked");
	    } else {
		LOGGER.error("STEP 16: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 17: Verify the client connected to ethernet has Internet Access
	     *
	     */
	    testStepNumber = "s17";
	    status = false;
	    connectedClientSettop = null;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info("STEP 17: DESCRIPTION: Verify the client connected to ethernet has Internet Access");
	    LOGGER.info("STEP 17: EXPECTED: Client connected to LAN should have Internet Access");
	    connectedClientSettop = BroadBandConnectedClientUtils.getEthernetConnectedClient(tapEnv, device);
	    result = BroadBandConnectedClientUtils.verifyInternetIsAccessibleInConnectedClientUsingCurl(tapEnv,
		    connectedClientSettop, BroadBandTestConstants.URL_W3SCHOOLS, BroadBandTestConstants.EMPTY_STRING);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info("STEP 17: ACTUAL: Client connected to ethernet has Internet Access");
	    } else {
		LOGGER.error("STEP 17: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);

	    /**
	     * Step 18: Verify the blocked website url cannot be accessible from the client connected to Ethernet
	     */
	    testStepNumber = "s18";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 18: DESCRIPTION: Verify the blocked website url cannot be accessible from the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 18: EXPECTED: Blocked website should not be accessible from the client connected to Ethernet");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_WIKIPEDIA);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 18: ACTUAL: Blocked Website is not accessible in the client connected to Ethernet from gateway");
	    } else {
		LOGGER.error("STEP 18: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	    /**
	     * Step 19: Verify the keyword added to blocked list can not be accessible from the client connected to
	     * Ethernet
	     *
	     */
	    testStepNumber = "s19";
	    status = false;
	    LOGGER.info("#####################################################################################");
	    LOGGER.info(
		    "STEP 19: DESCRIPTION: Verify the keyword added to blocked list can not be accessible from the client connected to Ethernet");
	    LOGGER.info(
		    "STEP 19: EXPECTED: Blocked keyword should not be accessible from the client connected to Ethernet");
	    result = BroadBandConnectedClientUtils.verifyInternetAccessUsingCurl(tapEnv, connectedClientSettop,
		    BroadBandTestConstants.URL_INSTAGRAM);
	    status = result.isStatus();
	    errorMessage = result.getErrorMessage();
	    if (status) {
		LOGGER.info(
			"STEP 19: ACTUAL: Keyword added to blocked site list is not accessible in the client connected to Ethernet from gateway, hence blocked");
	    } else {
		LOGGER.error("STEP 19: ACTUAL: " + errorMessage);
	    }
	    LOGGER.info("#####################################################################################");
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, false);

	} catch (Exception testException) {
	    errorMessage = testException.getMessage();
	    LOGGER.error(
		    "EXCEPTION OCCURRED WHILE BLOCKING THE INTERNET ACCESS TO THE BLOCKED SITE AND KEYWORD FOR A PARTICULAR DAY AND TIME PERIOD : "
			    + errorMessage);
	    tapEnv.updateExecutionStatus(device, testId, testStepNumber, status, errorMessage, true);
	} finally {
	    if (newRowsAdded != null && !newRowsAdded.isEmpty()) {
		LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
		LOGGER.info(
			"POST-CONDITION-1: DESCRIPTION: Verify the sites and keywords added to blocked sites table are removed");
		LOGGER.info(
			"POST-CONDITION-1: EXPECTED: Sites and keywords added to blocked site table should be removed");
		WebPaServerResponse webPaServerResponse = null;
		for (String row : newRowsAdded) {
		    webPaServerResponse = tapEnv.deleteTableRowUsingRestApi(device, row);
		}
		if (webPaServerResponse.getMessage().equalsIgnoreCase(BroadBandTestConstants.SUCCESS_TXT)) {
		    LOGGER.info(
			    "POST-CONDITION-1 PASSED: Sites and keywords added to blocked site table have been successfully removed");
		} else {
		    LOGGER.error(
			    "POST-CONDITION-1 FAILED: Sites and keywords added to blocked site table cannot be removed");
		}
		LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	    }

	    if (isEnabled) {
		LOGGER.info("########################### STARTING POST-CONFIGURATIONS ###########################");
		LOGGER.info(
			"POST-CONDITION 2: DESCRIPTION: Verify the Parental Control - Manage Sites is disabled via WebPA");
		LOGGER.info("POST-CONDITION 2: EXPECTED: Parental Control - Manage Sites should be disabled via WebPA");
		if (!BroadBandWebPaUtils.setAndGetParameterValuesUsingWebPa(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_TO_ENABLE_PARENTAL_CONTROLS,
			WebPaDataTypes.BOOLEAN.getValue(), BroadBandTestConstants.FALSE)) {
		    LOGGER.info(
			    "POST-CONDITION-2 FAILED: Parental Control - Manage Sites cannot be disabled via WebPa");
		} else {
		    LOGGER.info(
			    "POST-CONDITION-2 PASSED: Parental Control - Manage Sites has been successfully disabled via WebPA");
		}
		LOGGER.info("########################### COMPLETED POST-CONFIGURATIONS ###########################");
	    }
	    LOGGER.info("#################### ENDING TEST CASE: TC-RDKB-PC-MNG-SITE-1001 #####################");
	}
    }
}
