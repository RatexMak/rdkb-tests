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

package com.automatics.rdkb.tests.wifi.connectedclients;

import org.testng.annotations.Test;

import com.automatics.annotations.TestDetails;
import com.automatics.constants.AutomaticsConstants;
import com.automatics.constants.DataProviderConstants;
import com.automatics.device.Device;
import com.automatics.device.Dut;
import com.automatics.exceptions.TestException;
import com.automatics.rdkb.BroadBandTestGroup;
import com.automatics.rdkb.constants.BroadBandCommandConstants;
import com.automatics.rdkb.constants.BroadBandTestConstants;
import com.automatics.rdkb.constants.BroadBandWebPaConstants;
import com.automatics.rdkb.constants.RDKBTestConstants;
import com.automatics.rdkb.constants.RDKBTestConstants.WiFiFrequencyBand;
import com.automatics.rdkb.utils.wifi.BroadBandWiFiUtils;
import com.automatics.tap.AutomaticsTapApi;
import com.automatics.test.AutomaticsTestBase;
import com.automatics.utils.CommonMethods;
import com.automatics.rdkb.utils.wifi.connectedclients.BroadBandConnectedClientUtils;
import com.automatics.rdkb.constants.WebPaParamConstants.WebPaDataTypes;
import com.automatics.rdkb.utils.BroadBandCommonUtils;
import com.automatics.rdkb.utils.CommonUtils;
import com.automatics.rdkb.utils.webpa.BroadBandWebPaUtils;

public class BroadBandWifiSsidBroadcastStatusTest extends AutomaticsTestBase {

    /**
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Disable " Broadcast SSID" for 2.4Ghz WiFi network and verify disabled status from device device</li>
     * <li>Expected: The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to false</li>
     * <li>Step 2a: Verify the status of Broadcast SSID as "False" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as False</li>
     * <li>Step 2b: Disable and enable back the Device.WiFi.SSID.10001.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10001.Enable should return value true</li>
     * <li>Step 3: Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop</li>
     * <li>Expected:2.4Ghz wifi ssid should not be listed in available networks for client device</li>
     * <li>Step 4: Enable "Broadcast SSID" for 2.4Ghz WiFi network and verify status using WebPA request</li>
     * <li>Expected:The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to true</li>
     * <li>Step 5a: Check the status of Broadcast SSID as "True" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as True</li>
     * <li>Step 5b: Disable and enable back the Device.WiFi.SSID.10001.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10001.Enable should return value true</li>
     * <li>Step 6: Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop</li>
     * <li>Expected:2.4Ghz ssid should be listed in available networks for client device</li>
     * </ol>
     * 
     * @author Revanth.k
     * @refactor Athira
     * 
     * @param Dut
     *            {@link device}
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-2001")
    public void testVerifyWifiSsidBroadcastingStatusFor2GhzPrivateSsid(Dut device) {

	// variable to store the testcase id
	String testCaseId = "TC-RDKB-WIFI-201";
	// boolean variable to store the status of each step
	boolean status = false;
	// variable to store the error message
	String errorMessage = null;
	// variable to store the step number
	String step = "s1";
	// variable to store the connected client details
	Dut connectedClientDevice = null;
	// variable to store the status for setting precondition
	boolean statusForPreCondition = false;
	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2001");
	LOGGER.info(
		"TEST DESCRIPTION: Enable/Disable Broadcast SSID for 2.4Ghz WiFi network and verify disabled status");

	LOGGER.info("TEST STEPS : ");

	LOGGER.info("1. Disable  Broadcast SSID for 5Ghz WiFi network and verify disabled status from  device");
	LOGGER.info("2a. Verify the status of Broadcast SSID as \"Down\" using WebPA request.");
	LOGGER.info("2b. Disable and enable back the Device.WiFi.SSID.10001.Enable");
	LOGGER.info("3. Verify whether 2.4 GHz SSID is seen in in 5Ghz client laptop");
	LOGGER.info("4. Enable \"Broadcast SSID\" for 2.4Ghz WiFi network and verify status using WebPA request");
	LOGGER.info("5a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("5b. Disable and enable back the Device.WiFi.SSID.10001.Enable");
	LOGGER.info("6. Verify whether 2.4 GHz SSID is seen in in 5Ghz client laptop");

	LOGGER.info("#######################################################################################");

	try {
	    /*
	     * Pre-condition :
	     * 
	     * 1.Find a device which has 2.4GHz Wifi support 2.Set the ssid names in 2.4Ghz and 5Ghz to different values
	     */
	    LOGGER.info("***********EXECUTING PRECONDITIONS*********");
	    LOGGER.info("PreCondition 1:Change the ssid names in 2.4Ghz and 5Ghz to different values");
	    LOGGER.info("PreCondition 2:Get a 2.4Ghz wifi capable client device");
	    // Get a client of device with 2.4GHz radio support
	    statusForPreCondition = BroadBandConnectedClientUtils
		    .setPrivateWifiSsidNamesIn2GhzAnd5GhzToStandardValues(device, tapEnv);
	    if (!statusForPreCondition) {
		errorMessage = "Failed in setting ssid names to different values as Pre Condition";
		LOGGER.error(errorMessage);
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("Waiting for 2 Minutes after changing the Wifi SSID names");
	    tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);

	    try {
		// Get a client of device with 2.4GHz radio support
		connectedClientDevice = BroadBandConnectedClientUtils
			.get2GhzWiFiCapableClientDeviceAndConnectToAssociated2GhzSsid(device, tapEnv);
	    } catch (Exception exception) {
		errorMessage = exception.getMessage();
		LOGGER.error("Exception occured during execution = >" + errorMessage);
	    }

	    LOGGER.info("***********Completed Pre Conditions*********");
	    if (connectedClientDevice != null) {

		/*
		 * Step 1: Disable the Private 2.4Ghz SSID Broadcast status via WebPA
		 */
		step = "s1";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 1:  DESCRIPTION : Disable \"Private Broadcast SSID status\" for 2.4Ghz WiFi network");
		LOGGER.info("Step 1: ACTION : Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled set to false");
		LOGGER.info(
			"Expected:The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to false ");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Unable to disable 2.4GHz Private SSID Broadcast status using WebPA parameter - Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled";

		// Disable the WebPA parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());

		LOGGER.info("Step 1: Actual: "
			+ (status ? "The Broadcast SSID status value is set to false for 2.4GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to false
		LOGGER.info("wait for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 2: Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.
		 */

		step = "s2";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info(
			"Step 2a) :  DESCRIPTION :Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 2a) : ACTION : Get value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info(
			"Step 2a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled\" should return value as False ");

		LOGGER.info("Step 2b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10001.Enable");
		LOGGER.info(
			"Step 2a) : ACTION : First set Device.WiFi.SSID.10001.Enable with false, then set to value true");

		LOGGER.info("Expected: 2a) : Expected :2.4 GHZ private SSID should Disabled and enabled back");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Broadcast status of 2.4GHz private SSID is not disabled even after disabling via WebPA parameter - Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" value as False
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE);

		LOGGER.info("Step 2(a): Actual: " + (status
			? "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled WebPA parameter returns value as False."
			: errorMessage));

		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 2.4GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		LOGGER.info("Response of webpa disable command " + status);
		if (status) {
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		    LOGGER.info("Step 2(b): Actual: "
			    + (status ? "Disabled and enabled back the 2.4GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 3: Verify whether 2.4 GHz SSID is seen in 2.4Ghz client laptop
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s3";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 3:  DESCRIPTION : Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info(
			"Step 3: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 3: Expected: 2.4 GHz SSID is seen in in client laptop ");
		LOGGER.info(
			"*****************************************************************************************");

		/*
		 * Verify 2.4GHz is not available in the client device after disabling the SSID Advertisement Enabled
		 * value.
		 */

		String ssIdName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_2_GHZ);
		LOGGER.info("SSID name in 2.4Ghz band: " + ssIdName);

		errorMessage = "2.4 GHz is still available in the client device even after disabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, false);

		    LOGGER.info("Step 3: Actual: "
			    + (status ? "Successfully verified 2.4 GHz SSID is seen in in 2.4Ghz client laptop"
				    : errorMessage));
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		/*
		 * Step 4: Enable "Private Broadcast SSID status" for 2.4Ghz WiFi network
		 */

		step = "s4";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 4: DESCRIPTION : Enable \"Private Broadcast SSID status\" for 2.4Ghz WiFi network");
		LOGGER.info(
			"Step 4: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info(
			"Step 4: Expected: The value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled must be set to true ");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Not able to enabled 2.4 GHz SSID using Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled WEBPA parameter ";
		/*
		 * Enable SSID value for 2.4GHz network
		 */
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 4: Actual: "
			+ (status ? "Broadcast status is set to true for 2.4GHz WiFi network" : errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to true
		LOGGER.info("wait for one minute");
		tapEnv.waitTill(AutomaticsConstants.ONE_MINUTE_IN_MILLIS);

		/*
		 * Step 5: Verify the status of Broadcast SSID using WebPA request
		 */

		step = "s5";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info(
			"Step 5a) :  DESCRIPTION :Verify the Private 2.4 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 5a) : ACTION : Get value of Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled");
		LOGGER.info(
			"Step 5a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled\" should return value as True ");

		LOGGER.info("Step 5b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10001.Enable");
		LOGGER.info(
			"Step 5b) : ACTION : First set Device.WiFi.SSID.10001.Enable with false, then set to value true");

		LOGGER.info("Expected: 5b) : Expected :2.4 GHZ private SSID should Disabled and enabled back");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Broadcast SSID status obtained using WebPA Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled is NOT TRUE";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" value as True
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE);
		LOGGER.info("Step 5(a): Actual: Status of 2.4GHz SSID broadcast is " + status);

		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 2.4GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		if (status) {
		    LOGGER.info(" SSID Broadcast is disabled");
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		    LOGGER.info("Step 5(b): Actual: "
			    + (status ? "Disabled and enabled back the 2.4GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 6: Verify whether 2.4 GHz SSID is seen in 2.4Ghz client device
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s6";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 6:  DESCRIPTION : Verify whether 2.4 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info(
			"Step 6: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 6: Expected: 2.4 GHz SSID is seen in in client laptop ");
		LOGGER.info(
			"*****************************************************************************************");
		/*
		 * Verify 2.4GHz is seen in client devices after enabling the above value
		 */
		errorMessage = "2.4 GHz is not available in the client device network after enabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, true);

		    LOGGER.info("Step 6: Actual: "
			    + (status ? "Successfully verified 2.4 GHz SSID is seen in client device" : errorMessage));
		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);
	    } else {
		errorMessage = "Failed to find device with 2.4GHz Wifi support";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured during execution = >" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    // Enable the WebPA parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled"
	    BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-2001");
    }

    /**
     * 
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Step 1: Disable " Broadcast SSID" for 5Ghz WiFi network and verify disabled status from device</li>
     * <li>Expected: The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to false</li>
     * <li>Step 2a: Verify the status of Broadcast SSID as "Down" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" should
     * return value as False</li>
     * <li>Step 2b: Disable and enable back the Device.WiFi.SSID.10101.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10101.Enable should return value true</li>
     * <li>Step 3: Verify whether 5 GHz SSID is seen in in 5Ghz client laptop</li>
     * <li>Expected:5Ghz wifi ssid should not be listed in available networks for client device</li>
     * <li>Step 4: Enable "Broadcast SSID" for 5Ghz WiFi network and verify status using WebPA request</li>
     * <li>Expected:The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to true</li>
     * <li>Step 5a: Check the status of Broadcast SSID as "True" using WebPA request</li>
     * <li>Expected: WebPA request for the parameter "Device.WiFi.AccessPoint.10001.SSIDAdvertisementEnabled" should
     * return value as True</li>
     * <li>Step 5b: Disable and enable back the Device.WiFi.SSID.10101.Enable</li>
     * <li>Expected: Device.WiFi.SSID.10101.Enable should return value true</li>
     * <li>Step 6: Verify whether 5 GHz SSID is seen in in 5Ghz client laptop</li>
     * <li>Expected:5Ghz ssid should be listed in available networks for client device</li>
     * </ol>
     * 
     * @author Revanth.K
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFI-2002")
    public void testVerifyBroadBandWifiSsidStatusFor5GHz(Dut device) {

	// variable to store the testcase id
	String testCaseId = "TC-RDKB-WIFI-202";
	// boolean variable to store the status of each step
	boolean status = false;
	// variable to store the error message
	String errorMessage = null;
	// variable to store the step number
	String step = "s1";
	// variable to store the connected client details
	Dut connectedClientDevice = null;
	// variable to store the status for setting precondition
	boolean statusForPreCondition = false;

	LOGGER.info("#######################################################################################");
	LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFI-2002");
	LOGGER.info("TEST DESCRIPTION: Enable/Disable Broadcast SSID for 5Ghz WiFi network and verify disabled status");

	LOGGER.info("TEST STEPS : ");

	LOGGER.info("1. Disable  Broadcast SSID for 5Ghz WiFi network and verify disabled status from  device");
	LOGGER.info("2a. Verify the status of Broadcast SSID as \"Down\" using WebPA request.");
	LOGGER.info("2a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("2b. Disable and enable back the Device.WiFi.SSID.10101.Enable");
	LOGGER.info("3. Verify whether 5 GHz SSID is seen in in 5Ghz client laptop");
	LOGGER.info("4. Enable \"Broadcast SSID\" for 5Ghz WiFi network and verify status using WebPA request");
	LOGGER.info("5a. Check the status of Broadcast SSID as \"Up\" using WebPA request");
	LOGGER.info("5b. Disable and enable back the Device.WiFi.SSID.10101.Enable");
	LOGGER.info("6. Verify whether 5 GHz SSID is seen in in 5Ghz client laptop");

	LOGGER.info("#######################################################################################");

	try {

	    /*
	     * Pre-condition : 1.Find a device which has 5GHz Wifi support 2.Set the ssid names in 2.4Ghz and 5Ghz to
	     * different values
	     * 
	     */
	    LOGGER.info("***********EXECUTING PRECONDITIONS*********");
	    LOGGER.info("PreCondition 1:Change the ssid names in 2.4Ghz and 5Ghz to different values");
	    LOGGER.info("PreCondition 2:Get a 5Ghz wifi capable client device");
	    statusForPreCondition = BroadBandConnectedClientUtils
		    .setPrivateWifiSsidNamesIn2GhzAnd5GhzToStandardValues(device, tapEnv);
	    if (!statusForPreCondition) {
		errorMessage = "Failed in setting ssid names to different values as Pre Condition";
		LOGGER.error(errorMessage);
		throw new TestException(errorMessage);
	    }
	    LOGGER.info("waiting for two minutes after changing the ssid names");
	    tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
	    LOGGER.info("PRE-CONDITION: Actual: "
		    + (statusForPreCondition ? "ssid names in 2.4Ghz and 5Ghz changed to different values"
			    : errorMessage));
	    connectedClientDevice = BroadBandConnectedClientUtils
		    .get5GhzWiFiCapableClientDeviceAndConnectToAssociated5GhzSsid(device, tapEnv);
	    LOGGER.info("***********Completed PRECONDITIONS*********");
	    if (connectedClientDevice != null) {

		/*
		 * Step 1: Disable the Private 5Ghz SSID Broadcast status via WebPA
		 */

		step = "s1";
		status = false;

		String ssIdName = BroadBandConnectedClientUtils.getSsidNameFromGatewayUsingWebPaOrDmcli(device, tapEnv,
			WiFiFrequencyBand.WIFI_BAND_5_GHZ);

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 1:  DESCRIPTION : Disable \"Private Broadcast SSID status\" for 5Ghz WiFi network");
		LOGGER.info("Step 1: ACTION : Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled set to false");
		LOGGER.info(
			"Expected:The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to false ");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Unable to disable 5GHz Private SSID Broadcast status using WebPA parameter - Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled";

		LOGGER.info("SSID name for 5Ghz band: " + ssIdName);

		/*
		 * Disable Private Broadcast SSID status value for 5GHz network
		 */
		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());

		LOGGER.info(
			"Actual: " + (status ? "The Broadcast SSID status value is set to false for 5GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to false
		LOGGER.info("wait for 90 seconds ");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 2: Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.
		 */

		step = "s2";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info(
			"Step 2a) :  DESCRIPTION :Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 2a) : ACTION : Get value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info(
			"Step 2a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled\" should return value as False ");

		LOGGER.info("Step 2b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10101.Enable");
		LOGGER.info(
			"Step 2b) : ACTION : First set Device.WiFi.SSID.10101.Enable with false, then set to value true");

		LOGGER.info("Expected: 2b) : Expected :5 GHZ private SSID should Disabled and enabled back");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Broadcast status of 5GHz private SSID is not disabled even after disabling via WebPA parameter - Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" value as False
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.FALSE);

		LOGGER.info("Step 2(a): Actual: " + (status
			? "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled WebPA parameter returns value as False."
			: errorMessage));

		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 5GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		LOGGER.info("status of webpa disable command is : " + status);
		if (status) {
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());

		    LOGGER.info("Step 2(b): Actual: "
			    + (status ? "Disabled and enabled back 5 GHz SSID Broadcast" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 3: Verify whether 5 GHz SSID is seen in in 5Ghz client device
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for 5 minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s3";
		status = false;
		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 3:  DESCRIPTION : Verify whether 5 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info(
			"Step 3: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 3: Expected: 5 GHz SSID is seen in client laptop ");
		LOGGER.info(
			"*****************************************************************************************");

		/*
		 * Verify 5GHz is not available in the client device after disabling the SSID Advertisement Enabled
		 * value.
		 */
		errorMessage = "5 GHz is still available in the client device even after disabling the SSIDAdvertismentEnabledValue";
		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, false);

		    LOGGER.info("Step 3: Actual: "
			    + (status ? "Successfully verified 5 GHz SSID is seen in in 2.4Ghz client laptop"
				    : errorMessage));

		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}

		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

		/*
		 * Step 4: Enable "Private Broadcast SSID status" for 5Ghz WiFi network
		 */

		step = "s4";
		status = false;
		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 4: DESCRIPTION : Enable \"Private Broadcast SSID status\" for 5Ghz WiFi network");
		LOGGER.info(
			"Step 4: ACTION : Execute the command and check for presence of 5GHz SSID in command output.");
		LOGGER.info(
			"Step 4: Expected: The value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled must be set to true ");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Not able to enabled 5 GHz SSID using Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled WEBPA parameter";
		/*
		 * Enable SSID value for 5GHz network
		 */

		status = BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Step 4: Actual: "
			+ (status ? "The Private Broadcast SSID status value is set to true for 5GHz WiFi network"
				: errorMessage));
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		// wait for one minute for ssid advertisement status to turn to true
		LOGGER.info("wait for 90 seconds");
		tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);

		/*
		 * Step 5: Verify the status of Broadcast SSID using WebPA request
		 */

		step = "s5";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info(
			"Step 5a) :  DESCRIPTION :Verify the Private 5 GHz SSID Broadcast status is disabled or not via WebPA.");
		LOGGER.info("Step 5a) : ACTION : Get value of Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled");
		LOGGER.info(
			"Step 5a) : WebPA request for the parameter \"Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled\" should return value as True ");

		LOGGER.info("Step 5b) :  DESCRIPTION :Disable and enable back the Device.WiFi.SSID.10101.Enable");
		LOGGER.info(
			"Step 5b) : ACTION : First set Device.WiFi.SSID.10101.Enable with false, then set to value true");

		LOGGER.info("Expected: 5b) : Expected :5 GHZ private SSID should Disabled and enabled back");
		LOGGER.info(
			"*****************************************************************************************");

		errorMessage = "Broadcast SSID status obtained using WebPA parameter Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled is NOT TRUE";

		/*
		 * Get the value of "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled" value as True
		 */
		status = BroadBandCommonUtils.getWebPaValueAndVerify(device, tapEnv,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
			BroadBandTestConstants.TRUE);
		LOGGER.info("Step 5(a): Actual: Status of 5GHz SSID broadcast is " + status);

		BroadBandWiFiUtils.setWebPaParams(device,
			BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			BroadBandTestConstants.FALSE, WebPaDataTypes.BOOLEAN.getValue());
		LOGGER.info("Waiting for 2 minutes after disabling 5GHZ ssid");
		tapEnv.waitTill(AutomaticsConstants.TWO_MINUTES);
		if (status) {
		    LOGGER.info(" SSID Broadcast is disabled");
		    status = BroadBandWiFiUtils.setWebPaParams(device,
			    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ENABLED_STATUS,
			    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());
		    LOGGER.info("Step 5(b): Actual: "
			    + (status ? "Disabled and enabled back the 5GHz SSID" : errorMessage));
		}
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

		/*
		 * Step 6: Verify whether 5 GHz SSID is seen in client device after enabling the above value
		 */

		// Wait for some time to reflect the changes in client device
		LOGGER.info("Waiting for five minutes");
		tapEnv.waitTill(AutomaticsConstants.FIVE_MINUTES);

		step = "s6";
		status = false;

		LOGGER.info(
			"*****************************************************************************************");
		LOGGER.info("Step 6:  DESCRIPTION : Verify whether 5 GHz SSID is seen in in 2.4Ghz client laptop");
		LOGGER.info(
			"Step 6: ACTION : Execute the command and check for presence of 2.4GHz SSID in command output.");
		LOGGER.info("Step 6: Expected: 5 GHz SSID is seen in client laptop ");
		LOGGER.info(
			"*****************************************************************************************");

		/*
		 * Verify 5GHz is seen in client devices after enabling the above value
		 */
		errorMessage = "5 GHz is not available in the client device network after enabling the SSIDAdvertismentEnabledValue";

		try {

		    status = BroadBandConnectedClientUtils.verifySsidVisibilityInClientDevice(connectedClientDevice,
			    tapEnv, ssIdName, true);
		    LOGGER.info("Step 6: Actual: "
			    + (status ? "Successfully verified 5 GHz SSID is seen in client device" : errorMessage));

		} catch (Exception exception) {
		    LOGGER.error("Exception occurred during execution => " + exception.getMessage());
		}

		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    } else {
		errorMessage = "Failed to find client device with 5GHz wifi support";
		LOGGER.error(errorMessage);
		tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);
	    }

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error(errorMessage);
	    LOGGER.error("Exception occured while enabling/ disabling the SSID Advertisement value for 5 GHz network"
		    + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    // Enable the WebPA parameter "Device.WiFi.AccessPoint.10101.SSIDAdvertisementEnabled"
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_5_GHZ_PRIVATE_SSID_ADV_ENABLED,
		    BroadBandTestConstants.TRUE, WebPaDataTypes.BOOLEAN.getValue());

	    tapEnv.waitTill(BroadBandTestConstants.NINTY_SECOND_IN_MILLIS);
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFI-2002");
    }

    /**
     * Test case is to validate the failure authentication attempts
     * <p>
     * STEPS:
     * </p>
     * <ol>
     * <li>Pre condition: Get the raspberry pi device from connected client & check the certs directory is available
     * under /etc/wpa_supplicant and reboot the GW device</li>
     * <li>Step 1: Get 2.4GHz LNF SSID of ath10 interface from GW device</li>
     * <li>Expected: Should return 2.4GHz LNF SSID</li>
     * <li>Step 2: Get 2.4GHz LNF BSSID of ath10 interface from GW device</li>
     * <li>Expected: Should return 2.4GHz LNF BSSID</li>
     * <li>Step 3: add wpa_supplicant.conf file to Raspberry pi device & update the BSSID</li>
     * <li>Expected:Should copy files from VM to raspberry pi device & BSSID should be updated</li>
     * <li>Step 4: add the invalid certificates from VM to RPI device</li>
     * <li>Expected:certificate file should available in RPI device</li>
     * <li>Step 5: Disable 2.4GHz LNF SSID using WebPA operation</li>
     * <li>Expected: Should disable 2.4GHz LNF SSID</li>
     * <li>Step 6: Restart the hostapd process using WebPA</li>
     * <li>Expected:Should restart hostapd process</li>
     * <li>Step 7: Verify 2.4GHz LNF SSID Blacklisttabletimeout value for 300 seconds</li>
     * <li>Expected:Should update BlacklistTableTimeout value for LNF SSID</li>
     * <li>Step 8: Update 2.4GHz LNF SSID maximum failure authentication attempts to 3 using WebPA in GW device</li>
     * <li>Expected:Should update Maximum athentication attempts as 3</li>
     * <li>Step 9: Enable 2.4GHz LNF SSID using WebPA operation</li>
     * <li>Expected:Should enable 2.4GHz LNF SSID</li>
     * <li>Step 10: Restart the hostapd process using WebPA</li>
     * <li>Expected:Should restart hostapd process</li>
     * <li>Step 11: Initiate connection request from RPI to GW device using LNF SSID</li>
     * <li>Expected:Connection request should fail</li>
     * <li>Step 12: Initiate connection request from RPI to GW device after timeout using LNF SSID</li>
     * <li>Expected: Time difference should be 300 minutes</li>
     * </ol>
     * 
     * @author ArunKumar Jayachandran
     * @author Prince ArunRaj
     * @refactor Said Hisham
     * 
     * @param device
     *            {@link Dut}
     */

    @Test(dataProvider = DataProviderConstants.CONNECTED_CLIENTS_DATA_PROVIDER, dataProviderClass = AutomaticsTapApi.class, alwaysRun = true, enabled = true, groups = {
	    BroadBandTestGroup.WEBPA, BroadBandTestGroup.WIFI })
    @TestDetails(testUID = "TC-RDKB-WIFISSID-2003")
    public void testVerifyRadiusAuthenticationLimitation(Dut device) {

	// variable to store the Test case id
	String testCaseId = "TC-RDKB-WIFISSID-203";
	// boolean variable to store the status of each step
	boolean status = false;
	// variable to store the error message
	String errorMessage = null;
	// variable to store the step number
	String step = "s1";
	// variable to store the connected client details
	Dut raspberryPi = null;
	// variable to store the command to be executed in device
	String command = null;
	// variable to store the response on executing a command on device
	String response = null;
	// String to store the BSSID of 2.4GHz LNF SSID
	String BSSID = null;
	try {
	    LOGGER.info("################### STARTING PRE-CONFIGURATIONS ###################");
	    LOGGER.info("PRE-CONDITION STEPS");
	    LOGGER.info("1. Get the Raspberry pi device from connected client");
	    LOGGER.info("2. Verify certs directory is available under /etc/wpa_supplicant");
	    LOGGER.info("3. Clear the /rdklogs/logs/radiusauthd.log file from GW device");
	    LOGGER.info("4. Reboot the GW device ");

	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Get the Raspberry pi device from connected client");
	    LOGGER.info(
		    "PRE-CONDITION : ACTION : Verify in all connected client device by executing the command cat /proc/device-tree/model");
	    LOGGER.info("PRE-CONDITION : EXPECTED : It should return the Raspberry model");
	    raspberryPi = BroadBandConnectedClientUtils.getRaspberryPiFromConnectedClients(device, tapEnv);
	    LOGGER.info("PRE-CONDITION : ACTUAL: RaspberryPi device is available in connected client setup");

	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Verify certs directory is available under /etc/wpa_supplicant");
	    LOGGER.info("PRE-CONDITION : ACTION : execute ls /etc/wpa_supplicant");
	    LOGGER.info("PRE-CONDITION : EXPECTED : It should contain certs directory if not creating the directory");
	    // check the directory certs is available in RPI device
	    command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
		    BroadBandCommandConstants.PATH_WPA_SUPPLICANT);
	    response = tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
	    // create directory if certs directory is not available
	    if (!CommonUtils.isGivenStringAvailableInCommandOutput(response, BroadBandTestConstants.STRING_CERTS)) {
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SUDO,
			BroadBandCommandConstants.CMD_MKDIR, BroadBandCommandConstants.PATH_CERTIFICATE);
		tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
	    }
	    LOGGER.info("PRE-CONDITION : ACTUAL: certs directory is available under /etc/wpa_supplicant");

	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Clear the /rdklogs/logs/radiusauthd.log file from GW device");
	    LOGGER.info("PRE-CONDITION : ACTION : execute echo \"\" > /rdklogs/logs/radiusauthd.log ");
	    LOGGER.info("PRE-CONDITION : EXPECTED : It should clear the log file");
	    // clear the radiusauthd.log file
	    if (CommonUtils.clearLogFile(tapEnv, device, BroadBandCommandConstants.FILE_RADIUSAUTHD_LOG)) {
		LOGGER.info("PRE-CONDITION : ACTUAL: Successfully cleared the log file /rdklogs/logs/radiusauthd.log");
		status = true;
	    }

	    LOGGER.info("PRE-CONDITION : DESCRIPTION : Reboot the GW device");
	    LOGGER.info("PRE-CONDITION : ACTION : execute reboot ");
	    LOGGER.info("PRE-CONDITION : EXPECTED : It should reboot the GW device");
	    // reboot the GW device
	    BroadBandCommonUtils.rebootDeviceAsPreCondition(tapEnv, device);
	    LOGGER.info("PRE-CONDITION : ACTUAL: Successfully rebooted the GW device");
	    LOGGER.info("PRE-CONFIGURATIONS : FINAL STATUS -  " + status);
	    LOGGER.info("################### COMPLETED PRE-CONFIGURATIONS ###################");

	    LOGGER.info("#######################################################################################");
	    LOGGER.info("STARTING TEST CASE: TC-RDKB-WIFISSID-2003");
	    LOGGER.info("TEST DESCRIPTION: Verify Radius authentication failure limitations ");
	    LOGGER.info("TEST STEPS : ");
	    LOGGER.info("1. Get 2.4GHz LNF SSID of ath10 interface from GW device");
	    LOGGER.info("2. Get 2.4GHz LNF BSSID of ath10 interface from GW device");
	    LOGGER.info("3. add wpa_supplicant.conf file to Raspberry pi device & update the BSSID");
	    LOGGER.info("4. add invalid certificates to RPI device");
	    LOGGER.info("5. Disable 2.4GHz LNF SSID using WebPA operation");
	    LOGGER.info("6. Restart the hostapd process using WebPA");
	    LOGGER.info("7. Verify 2.4GHz LNF SSID Blacklisttabletimeout value for 300 seconds");
	    LOGGER.info(
		    "8. Update 2.4GHz LNF SSID maximum failure authentication attempts to 3 using WebPA in GW device");
	    LOGGER.info("9. Enable 2.4GHz LNF SSID using WebPA operation");
	    LOGGER.info("10. Restart the hostapd process using WebPA");
	    LOGGER.info("11. Initiate connection request from RPI to GW device using LNF SSID");
	    LOGGER.info("12. Initiate connection request from RPI to GW device after timeout using LNF SSID");
	    LOGGER.info("#######################################################################################");

	    /*
	     * Step 1: Get 2.4GHz LNF SSID of ath10 interface from GW device
	     */
	    step = "s1";
	    status = false;

	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 1: DESCRIPTION : Get 2.4GHz LNF SSID of ath10 interface from GW device ");
	    LOGGER.info("STEP 1: ACTION : Execute WebPa command for Device.WiFi.SSID.10006.SSID ");
	    LOGGER.info("STEP 1: EXPECTED : It Should return 2.4GHz LNF SSID as WebPa response ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to get the 2.4GHz LNF SSID for Device.WiFi.SSID.10006.SSID WebPA parameter using WebPA";
	    // Get the LNF SSID from GW device using WebPA get operation
	    response = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4GHZ_LNF_SSID);
	    status = CommonMethods.isNotNull(response);
	    LOGGER.info("STEP1: "
		    + (status ? "ACTUAL: Successfully received 2.4GHz LNF SSID name of ath10 interface: " + response
			    : errorMessage + " and the response: " + response));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 2: Get 2.4GHz LNF BSSID of ath10 interface from GW device
	     */
	    step = "s2";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 2: DESCRIPTION : Get 2.4GHz LNF BSSID of ath10 interface from GW device ");
	    LOGGER.info("STEP 2: ACTION : Execute WebPa command for Device.WiFi.SSID.10006.BSSID ");
	    LOGGER.info("STEP 2: EXPECTED : It Should return 2.4GHz LNF BSSID as WebPa response ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to get the 2.4GHz LNF BSSID for Device.WiFi.SSID.10006.BSSID WebPA parameter using WebPA";
	    // Get the LNF BSSID from GW device using WebPA get operation
	    BSSID = tapEnv.executeWebPaCommand(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4GHZ_LNF_BSSID);
	    status = CommonMethods.isNotNull(BSSID);
	    LOGGER.info(
		    "STEP2: " + (status ? "ACTUAL: Successfully received LNF BSSID name of ath10 interface: " + response
			    : errorMessage + " and the response: " + response));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /*
	     * Step 3: Copy wpa_supplicant.conf file from VM to RPI device & update the BSSID in wpa_supplicant.conf
	     * file
	     */
	    step = "s3";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 3: DESCRIPTION : Add wpa_supplicant.conf file to Raspberry pi device & update the BSSID ");
	    LOGGER.info(
		    "STEP 3: ACTION : add wpa_supplicant.conf file to /etc/wpa_supplicant/ & update BSSID with sed command ");
	    LOGGER.info("STEP 3: EXPECTED : wpa_supplicant.conf file should update with current BSSID ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to copy files from VM to RPI device";

	    tapEnv.copyFileToDevice((Device) raspberryPi, BroadBandCommandConstants.FILE_WPA_SUPPLICANT_CONF,
		    "/etc/wpa_supplicant");

	    command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_LS,
		    BroadBandCommandConstants.PATH_WPA_SUPPLICANT);
	    response = tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
	    errorMessage = "Failed to move wpa_supplicant.conf file to RPI device under /etc/wpa_supplicant";

	    // verify wpa_supplicant.conf file is available in RPI device
	    if (CommonMethods.isNotNull(response) && CommonUtils.isGivenStringAvailableInCommandOutput(response,
		    BroadBandCommandConstants.FILE_WPA_SUPPLICANT_CONF)) {
		errorMessage = "Failed to updated BSSID value in /etc/wpa_supplicant/wpa_supplicant.conf";
		// get the current BSSID value from wpa_supplicant.conf file
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SUDO,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.STRING_BSSID,
			BroadBandCommandConstants.PATH_WPA_SUPPLICANT,
			BroadBandCommandConstants.FILE_WPA_SUPPLICANT_CONF);
		response = tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
		// command to replace BSSID value in same
		// wpa_supplicant.conf file
		response = CommonMethods.patternFinder(response,
			BroadBandTestConstants.PATTERN_BSSID_IN_WPA_SUPPLICANT);
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SUDO,
			BroadBandCommandConstants.CMD_SED, response, BroadBandTestConstants.STRING_BSSID_EQUAL, BSSID,
			BroadBandTestConstants.STRING_G, BroadBandCommandConstants.PATH_WPA_SUPPLICANT,
			BroadBandCommandConstants.FILE_WPA_SUPPLICANT_CONF);
		tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
		// Verify BSSID updated with current GW device BSSID
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SUDO,
			BroadBandTestConstants.GREP_COMMAND, BroadBandTestConstants.STRING_BSSID,
			BroadBandCommandConstants.PATH_WPA_SUPPLICANT,
			BroadBandCommandConstants.FILE_WPA_SUPPLICANT_CONF);
		response = tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response, BSSID);
	    }
	    // }
	    LOGGER.info("STEP3: " + (status ? "ACTUAL: Updated GW BSSID in RPI device successfully " : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /*
	     * Step 4: Copy the invalid certificates from VM to RPI device
	     */
	    step = "s4";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 4: DESCRIPTION : Copy the invalid certificates from VM to RPI device ");
	    LOGGER.info(
		    "STEP 4: ACTION : Execute echo >> <certificate> /etc/wpa_supplicant/certs/<FileName> <RPI device> ");
	    LOGGER.info("STEP 4: EXPECTED : certificate file should available in RPI device ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to add certificate files in location /etc/wpa_supplicant/certs in RPI device";
	    // add the certificate files to /etc/wpa_supplicant/certs/
	    status = BroadBandCommonUtils.addInvalidCertificatedsToRPI(tapEnv, raspberryPi);
	    LOGGER.info(
		    "STEP4: " + (status ? "Certificate files are added to RPI device successfully " : errorMessage));
	    if (status) {
		errorMessage = "Certificate files are not available in RPI device /etc/wpa_supplicant/certs";
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(BroadBandCommandConstants.CMD_SUDO,
			BroadBandCommandConstants.CMD_MV, BroadBandCommandConstants.FILE_XI5_CLIENT_KEY,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_XI5_CLIENT_CERT,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.FILE_CA_CHAIN_CERT,
			BroadBandTestConstants.SINGLE_SPACE_CHARACTER, BroadBandCommandConstants.PATH_CERTIFICATE);
		tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
		// verify certificates are moved to /etc/wpa_supplicant/certs
		command = BroadBandCommonUtils.concatStringUsingStringBuffer(
			BroadBandCommandConstants.CMD_LS + BroadBandCommandConstants.PATH_CERTIFICATE);
		response = tapEnv.executeCommandOnOneIPClients(raspberryPi, command);
		status = CommonMethods.isNotNull(response)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandCommandConstants.FILE_XI5_CLIENT_KEY)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandCommandConstants.FILE_XI5_CLIENT_CERT)
			&& CommonUtils.isGivenStringAvailableInCommandOutput(response,
				BroadBandCommandConstants.FILE_CA_CHAIN_CERT);
	    }
	    LOGGER.info("STEP4: " + (status ? "ACTUAL: Certificate files are available in /etc/wpa_supplicant/certs  "
		    : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, true);

	    /*
	     * Step 5: Disable 2.4GHz LNF SSID using WebPA operation
	     */
	    step = "s5";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 5: DESCRIPTION : Disable 2.4GHz LNF SSID using WebPA operation ");
	    LOGGER.info(
		    "STEP 5: ACTION : Execute WebPa set command for Device.WiFi.SSID.10006.Enable with value as false ");
	    LOGGER.info("STEP 5: EXPECTED : It should disable 2.4GHz LNF SSID ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to disable 2.4GHz LNF SSID using the WebPA parameter Device.WiFi.SSID.10006.Enable";
	    // Disable 2.4GHz LNF SSID using WebPA
	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_LNF_SSID_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.FALSE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP5: " + (status ? "ACTUAL: 2.4GHz LNF SSID is disabled properly" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 6: Restart the hostapd process using WebPA
	     */
	    step = "s6";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 6: DESCRIPTION : Restart the hostapd process using WebPA ");
	    LOGGER.info(
		    "STEP 6: ACTION : Execute WebPa set command for Device.WiFi.Radio.10000.X_CISCO_COM_ApplySetting with value as true ");
	    LOGGER.info("STEP 6: EXPECTED : It should restart hostapd process ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to set Device.WiFi.Radio.10000.X_CISCO_COM_ApplySetting WebPA parameter as true using WebPA command";
	    // Enable the apply setting for 2.4GHz
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_APPLY_SETTING, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("STEP6: " + (status ? "ACTUAL: Hostapd process is restarted properly" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 7: update & Verify 2.4GHz LNF SSID BlacklistTableTimeout value for 300 seconds
	     */
	    step = "s7";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 7: DESCRIPTION : Verify 2.4GHz LNF SSID Blacklisttabletimeout value for 300 seconds ");
	    LOGGER.info(
		    "STEP 7: ACTION : Execute WebPa set command for Device.WiFi.AccessPoint.10006.Security.X_COMCAST-COM_RadiusSettings.BlacklistTableTimeout with value as 300 ");
	    LOGGER.info("STEP 7: EXPECTED : It should update BlacklistTableTimeout value for LNF SSID ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to update Device.WiFi.AccessPoint.10006.Security.X_COMCAST-COM_RadiusSettings.BlacklistTableTimeout WebPA parameter";
	    // Get the Block time from GW device using WebPA get operation
	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4GHZ_LNF_BLACK_LIST_TABLE_TIMEOUT,
		    BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_300,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info(
		    "STEP7: " + (status ? "ACTUAL: BlacklistTableTimeout is updated with 300 seconds" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 8: update & Verify 2.4GHz LNF SSID Maximum radius authentication attempts as 3
	     */
	    step = "s8";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info(
		    "STEP 8: DESCRIPTION : Update 2.4GHz LNF SSID maximum failure authentication attempts to 3 using WebPA in GW device ");
	    LOGGER.info(
		    "STEP 8: ACTION : Execute WebPa set command for Device.WiFi.AccessPoint.10006.Security.X_COMCAST-COM_RadiusSettings.MaxAuthenticationAttempts with value as 3 ");
	    LOGGER.info("STEP 8: EXPECTED : It should update Maximum athentication attempts as 3 ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to update Device.WiFi.AccessPoint.10006.Security.X_COMCAST-COM_RadiusSettings.MaxAuthenticationAttempts WebPA parameter";
	    // update and verify the WebPA parameter
	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4GHZ_LNF_MAX_AUTHENTICATION_ATTEMPTS,
		    BroadBandTestConstants.CONSTANT_1, BroadBandTestConstants.STRING_VALUE_THREE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP8: " + (status ? "ACTUAL: Maxmium athentication attempts updated as 3" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 9: Enable 2.4GHz LNF SSID using WebPA
	     */
	    step = "s9";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 9: DESCRIPTION : Enable 2.4GHz LNF SSID using WebPA operation ");
	    LOGGER.info(
		    "STEP 9: ACTION : Execute WebPa set command for Device.WiFi.SSID.10006.Enable with value as true ");
	    LOGGER.info("STEP 9: EXPECTED : It should disable 2.4GHz LNF SSID ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to enable 2.4GHz LNF SSID using the WebPA parameter Device.WiFi.SSID.10006.Enable";
	    // Enable 2.4GHz LNF SSID using WebPA
	    status = BroadBandWebPaUtils.verifyWebPaValueAfterDuration(device, tapEnv,
		    BroadBandWebPaConstants.WEBPA_PARAM_DEVICE_WIFI_2_4_GHZ_LNF_SSID_ENABLED,
		    BroadBandTestConstants.CONSTANT_3, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.ONE_MINUTE_IN_MILLIS);
	    LOGGER.info("STEP9: " + (status ? "2.4GHz LNF SSID is enabled using WebPA properly" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	    /*
	     * Step 10: Restart the hostapd process using WebPA
	     */
	    step = "s10";
	    status = false;
	    LOGGER.info("**********************************************************************************");
	    LOGGER.info("STEP 10: DESCRIPTION : Restart the hostapd process using WebPA ");
	    LOGGER.info(
		    "STEP 10: ACTION : Execute WebPa set command for Device.WiFi.Radio.10000.X_CISCO_COM_ApplySetting with value as true ");
	    LOGGER.info("STEP 10: EXPECTED : It should restart hostapd process ");
	    LOGGER.info("**********************************************************************************");

	    errorMessage = "Failed to set Device.WiFi.Radio.10000.X_CISCO_COM_ApplySetting value as true using WebPA command";
	    // Restart hostapd process using WebPA set operation
	    status = BroadBandWiFiUtils.setWebPaParams(device,
		    BroadBandWebPaConstants.WEBPA_PARAM_WIFI_2_4_APPLY_SETTING, BroadBandTestConstants.TRUE,
		    BroadBandTestConstants.CONSTANT_3);
	    LOGGER.info("STEP10: " + (status ? "ACTUAL: Hostapd process restarted using WebPA" : errorMessage));
	    tapEnv.updateExecutionStatus(device, testCaseId, step, status, errorMessage, false);

	} catch (Exception exception) {
	    errorMessage = exception.getMessage();
	    LOGGER.error("Exception occured during Radius authentication limitation verification" + errorMessage);
	    CommonUtils.updateTestStatusDuringException(tapEnv, device, testCaseId, step, status, errorMessage, true);
	} finally {
	    LOGGER.info("################### STARTING POST-CONFIGURATIONS ###################");
	    LOGGER.info("POST-CONDITION : DESCRIPTION : Kill the wpa_supplicant process in RPI device ");
	    LOGGER.info("POST-CONDITION : ACTION : Execute the command kill -9 <pid> ");
	    LOGGER.info("POST-CONDITION : EXPECTED : It should kill the wpa_supplicant process ");
	    // kill the wpa_supplicant process in RPI device
	    status = BroadBandConnectedClientUtils.killProcessInConnectedClientSettop(raspberryPi, tapEnv,
		    BroadBandTestConstants.STRING_WPA_SUPPLICANT);

	    LOGGER.info(
		    (status ? "POST-CONDITION : ACTUAL: Successfully killed the wpa_supplicant process in RPI device"
			    : "Failed to kill the wpa_supplicant process"));

	    // remove certificates added to the RPI
	    status = BroadBandCommonUtils.rmInvalidCertificatedsToRPI(tapEnv, raspberryPi);
	    LOGGER.info("POST-CONFIGURATIONS : FINAL STATUS - " + status);

	    LOGGER.info("################### COMPLETED POST-CONFIGURATIONS ###################");
	}
	LOGGER.info("ENDING TEST CASE: TC-RDKB-WIFISSID-2003");
    }
}
